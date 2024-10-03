import munit.FunSuite
import parser.Parser

import cats.implicits.*

class SimpleTests extends FunSuite {

  test("monoid") {
    val digit: Parser[Int] =
      List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
        .foldLeft(Parser.fail[Int]){ (accum, digit) =>
          accum.orElse(Parser.string(digit.toString).as(digit))
        }

    val res = digit.parse("4b5")
    println(res)
  }

}
