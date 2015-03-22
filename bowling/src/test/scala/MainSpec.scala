package com.potix2.kata.bowling.bowling

import org.specs2.mutable._
import scala.util.parsing.combinator.RegexParsers

object Bowling {
  object ScoreParser extends RegexParsers {
    def gutter: Parser[Int] = """\-""".r ^^ {_ => 0}
    def digit: Parser[Int] = """\d""".r ^^ {_.toInt}
    def role: Parser[Int] = digit | gutter
    def strike: Parser[Frame] = "X".r ^^ {_ => Strike()}
    def normal: Parser[Frame] = role ~ role ^^ {x => NormalFrame(x._1, x._2)}
    def spare: Parser[Frame] = role <~ '/' ^^ { Spare }
    def frame: Parser[Frame] = strike | spare | normal
    def lastFrame: Parser[List[Frame]] = normal ^^ { List(_) } |
      ('X' ~ role ~ role) ^^ {x => List(Strike(), LastFrame(x._1._2, x._2))} |
      ('X' ~ role ~ '/') ^^ {x => List(Strike(), LastFrame(x._1._2, 10 - x._1._2)) } |
      ('X' ~ 'X' ~ 'X') ^^ {x => List(Strike(), LastFrame(10,10)) } |
      (spare ~ role) ^^ {x => List(x._1, LastFrame(x._2, 0)) }

    def game: Parser[List[Frame]] = repN(9, frame) ~ lastFrame ^^ {x => x._1 ++ x._2}
  }

  trait Frame {
    def r1: Int
  }

  case class NormalFrame(r1: Int, r2: Int) extends Frame
  case class LastFrame(r1: Int, r2: Int) extends Frame

  case class Spare(r1: Int) extends Frame {
    val r2: Int = 10 - r1
  }

  case class Strike() extends Frame {
    def r1: Int = 10
  }

  def score(indicators: String): Int = {
    val result = ScoreParser.parseAll(ScoreParser.game, indicators)
    result.get
      .zip(result.get.drop(1) ++ List(NormalFrame(0, 0)))
      .zip(result.get.drop(2) ++ List(NormalFrame(0, 0), NormalFrame(0, 0)))
      .map(x => (x._1._1, x._1._2, x._2))
      .foldRight(0) { (a,b) =>
      a match {
        case (f: NormalFrame, _, _) => b + f.r1 + f.r2
        case (f1: Spare,  f2: Frame, _) =>          b + f1.r1 + f1.r2 + f2.r1
        case (f1: Strike, f2: Spare, _) =>          b + f1.r1 + f2.r1 + f2.r2
        case (f1: Strike, f2: NormalFrame, _) =>    b + f1.r1 + f2.r1 + f2.r2
        case (f1: Strike, f2: LastFrame, _) =>      b + f1.r1 + f2.r1 + f2.r2
        case (f1: Strike, f2: Strike, f3: Frame) => b + f1.r1 + f2.r1 + f3.r1
        case _ => b
      }
    }
  }
}

class MainSpec extends SpecificationWithJUnit {
  "Bowling" should {
    "calculate score 11111111111111111111" in {
      Bowling.score("11111111111111111111") must_== 20
    }

    "calculate score 22222222222222222222" in {
      Bowling.score("22222222222222222222") must_== 40
    }

    "calculate score --------------------" in {
      Bowling.score("--------------------") must_== 0
    }

    "calculate score 9-9-9-9-9-9-9-9-9-9-" in {
      Bowling.score("9-9-9-9-9-9-9-9-9-9-") must_== 90
    }

    "calculate score 9/9-9-9-9-9-9-9-9-9-" in {
      Bowling.score("9/9-9-9-9-9-9-9-9-9-") must_== 100
    }

    "calculate score 1/2/36--------------" in {
      Bowling.score("1/2/36--------------") must_== 12 + 13 + 9
    }

    "calculate score X------------------" in {
      Bowling.score("X------------------") must_== 10
    }

    "calculate score X12----------------" in {
      Bowling.score("X12----------------") must_== 16
    }

    "calculate score X1/2---------------" in {
      Bowling.score("X1/2---------------") must_== 34
    }

    "calculate score XXX--------------" in {
      Bowling.score("XXX--------------") must_== 60
    }

    "calculate score XXX12------------" in {
      Bowling.score("XXX12------------") must_== 30 + 21 + 13 + 3
    }

    "calculate score 5/5/5/5/5/5/5/5/5/5/-" in {
      Bowling.score("5/5/5/5/5/5/5/5/5/5/-") must_== 145
    }

    "calculate score 5/5/5/5/5/5/5/5/5/5/5" in {
      Bowling.score("5/5/5/5/5/5/5/5/5/5/5") must_== 150
    }

    "calculate score XXXXXXXXXXXX" in {
      Bowling.score("XXXXXXXXXXXX") must_== 300
    }

    "calculate score XXXXXXXXXX--" in {
      Bowling.score("XXXXXXXXXX--") must_== 270
    }

    "calculate score XXXXXXXXXX1-" in {
      Bowling.score("XXXXXXXXXX1-") must_== 272
    }

    "calculate score XXXXXXXXXX5/" in {
      Bowling.score("XXXXXXXXXX5/") must_== 285
    }

    "calculate score XXXXXXXXX5/-" in {
      Bowling.score("XXXXXXXXX5/-") must_== 265
    }

    "calculate score XXXXXXXXX5/1" in {
      Bowling.score("XXXXXXXXX5/1") must_== 266
    }

  }
}
