import munit.FunSuite
import parser.{Album, Parser, Success}
import cats.implicits.*
import munit.Assertions.*

import scala.language.postfixOps

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

  test("applicative1") {
    val stringParser: Parser[String] = Parser.string("abc")
    val intParser: Parser[Int] = Parser.string("1").map(_.toInt)

    val albumParser = stringParser
      .product(stringParser)
      .product(intParser)
      .map{ case ((a, n), y) => Album(a, n, y) }

    val res = albumParser.parse("abcabc1")
    println(res)
  }

  test("applicative2") {
    val stringParser: Parser[String] = Parser.string("abc")
    val intParser: Parser[Int] = Parser.string("1").map(_.toInt)

    val albumParser = (stringParser, stringParser, intParser)
      .mapN((a, n, y) => Album(a, n, y))

    val input = "abcabc1"
    val res = albumParser.parse(input)

    assertEquals(res, Success(Album("abc", "abc", 1), input, 7))
  }

  test("flatMap") {
    val fieldParser: Parser[String] = Parser.fields() // parses <type>: and returns <type>
    val intParser: Parser[Int] = Parser.int(100)
    val stringParser: Parser[String] = Parser.string("str")

    val parser: Parser[Any] = fieldParser.flatMap(result =>
      result match {
        case "number" => intParser
        case "string" => stringParser
      }
    )
    val res = parser.parse("number: 100string: str")

    println(res)
  }


}
