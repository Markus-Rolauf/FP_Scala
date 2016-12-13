
package parsers

import language.higherKinds
import language.implicitConversions

trait Parsers[ParseError, Parser[+_]] { self =>

  implicit def char(c : Char) : Parser[Char]
  implicit def string(s : String) : Parser[String]
  implicit def operators[A](p : Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a : A)(implicit f : A => Parser[String]) : ParserOps[String] = ParserOps(f(a))

  def run[A](p : Parser[A])(input : String) : Either[ParseError, A]
  def or[A](s1 : Parser[A], s2 : Parser[A]) : Parser[A]
  def many[A](p : Parser[A]) : Parser[List[A]]
  def map[A, B](a : Parser[A])(f : A => B) : Parser[B]
  def slice[A](p : Parser[A]) : Parser[String]
  def product[A, B](p1 : Parser[A])(p2 : => Parser[B]) : Parser[(A, B)]
  def map2[A, B, C](p1 : Parser[A])(p2 : => Parser[B]) : Parser[C]
  def defaultSucceed[A](a : A) : Parser[A] =
    string("") map (_ => a)
  def succeed[A](a : A) : Parser[A]
  def flatMap[A, B](p : Parser[A])(f : A => Parser[B]) : Parser[B]

  case class ParserOps[A](p : Parser[A]) {
    def |[B >: A](p2 : Parser[B]) : Parser[B] = self.or(p, p2)
    def or[B >: A](p2 : Parser[B]) : Parser[B] = self.or(p, p2)
    def map[B](f : A => B) : Parser[B] = self.map(p)(f)
  }
}