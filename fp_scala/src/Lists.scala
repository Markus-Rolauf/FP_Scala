import scala.collection.immutable.Stream.Cons
import scala.collection.immutable.Nil
import scala.annotation.tailrec

object Lists {
  def main(args : Array[String]) : Unit = {

    println(apppend(List(1, 2, 3), 4))
  }

  def tail[A](list : List[A]) : List[A] =
    list match {
      case Nil => Nil
      case h :: t => t
    }

  def setHead[A](elem : A, list : List[A]) : List[A] =
    list match {
      case Nil => Nil
      case h :: t => elem :: t
    }

  def drop[A](list : List[A], count : Int) : List[A] =
    if (count == 0) list

    else
      list match {
        case Nil => Nil
        case h :: t => drop(t, count - 1)
      }

  def dropWhile[A](list : List[A], func : A => Boolean) : List[A] =
    list match {
      case Nil => Nil
      case h :: t => if (func(h)) dropWhile(list, func) else list
    }

  def length[A](list : List[A]) : Int =
    list.foldRight(0)((_, n) => 1 + n)

  def length2[A](list : List[A]) : Int =
    list.foldLeft(0)((n, _) => 1 + n)

  @tailrec
  def foldL[A, B](as : List[A], acc : B)(f : (B, A) => B) : B =
    as match {
      case Nil => acc
      case head :: tail => foldL(tail, f(acc, head))(f)

    }

  def reverse[A](as : List[A]) : List[A] =
    as.foldLeft(List[A]())((b, a) => a :: b)

  def apppend[A](as : List[A], a : A) : List[A] =
    as.foldRight(List[A](a))((org, acc) => org :: acc)

}