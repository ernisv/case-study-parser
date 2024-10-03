/*
 * Copyright 2022 Creative Scala
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package parser

import cats.implicits.toFunctorOps
import cats.kernel.Eq
import cats.{Applicative, Functor, Monoid, Semigroupal}

sealed trait Parser[A] {
  import Parser._

  def orElse(p: Parser[A]): Parser[A] = OrElseParser(this, p)

  def map[B](f: A => B): Parser[B] =
    ParserMap(this, f)

  def flatMap[B](f: A => Parser[B]): Parser[B] = ParserFlatMap(this, f)

  def parse(input: String): Result[A] = {
    def loop[A](parser: Parser[A], index: Int): Result[A] =
      parser match {
        case ConstParser(c: A) =>
          Success(c, input, index)

        case ParserFlatMap(source, f) =>
          loop(source, index) match {
            case Failure(reason, input, start) => Failure(reason, input, start)
            case Success(res, _, offset) =>
              loop(f(res), offset)
          }

        case ProductParser(pa, pb) =>
          loop(pa, index) match {
            case Success(resA, input, offsetA) =>
              loop(pb, offsetA) match {
                case Success(resB, input, offsetB) =>
                  Success((resA, resB), input, offsetB)
                case Failure(reason, input, start) => Failure(reason, input, start)
              }
            case Failure(reason, input, start) => Failure(reason, input, start)
          }

        case FailingParser() =>
          Failure[A]("failure", input, 0)

        case OrElseParser(source, otherwise) =>
          loop(source, index) match {
            case _: Failure[A] => loop(otherwise, index)
            case success: Success[A] => success
          }

        case ParserMap(source, f) =>
          loop(source, index) match {
            case Failure(reason, input, start) => Failure(reason, input, start)
            case Success(result, input, offset) =>
              Success(f(result), input, offset)
          }

        case ParserString(value) =>
          if (input.startsWith(value, index))
            Success(value, input, index + value.size)
          else
            Failure(
              s"input did not start with $value at index $index",
              input,
              index
            )

        case _: FieldsParser =>
          def checkKnownType(typeStr: String): Option[Result[String]] = {
            if (input.startsWith(s"${typeStr}: "))
              Some(Success(typeStr, input, index + typeStr.length + 2))
            else None
          }
          checkKnownType("number")
          .orElse(checkKnownType("string"))
            .getOrElse(Failure("Field type not known", input, index))
      }

    loop(this, 0)
  }
}
object Parser {
  def string(value: String): Parser[String] = ParserString(value)

  def int(number: Int): Parser[Int] = Parser.string(number.toString).as(number)

  def fields(): Parser[String] = FieldsParser()

  def fail[A]: Parser[A] = FailingParser()

  final case class FailingParser[A]() extends Parser[A]
  final case class ConstParser[A](const: A) extends Parser[A]
  final case class OrElseParser[A](source: Parser[A], otherwise: Parser[A]) extends Parser[A]
  final case class ParserString(value: String) extends Parser[String]
  final case class ParserMap[A, B](source: Parser[A], f: A => B) extends Parser[B]
  final case class ProductParser[A, B](pa: Parser[A], pb: Parser[B]) extends Parser[(A, B)]
  final case class FieldsParser() extends Parser[String]
  final case class ParserFlatMap[A, B](source: Parser[A], f: A => Parser[B]) extends Parser[B]

  implicit val parserFunctorInstance: Functor[Parser] =
    new Functor[Parser] {
      def map[A, B](fa: Parser[A])(f: A => B): Parser[B] =
        fa.map(f)
    }

  implicit val parserApplicative: Semigroupal[Parser] = new Semigroupal[Parser] {
    override def product[A, B](fa: Parser[A], fb: Parser[B]): Parser[(A, B)] = ProductParser(fa, fb)
  }

}
