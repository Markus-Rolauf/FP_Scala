import scala.collection.immutable.Stream.Cons
import scala.collection.immutable.Nil
import scala.annotation.tailrec

object Lists {
  def main(args : Array[String]) : Unit = {

    println(hasSubsequence(List[Int](1, 3, 1, 2, 7, 8), List[Int](1, 2)))
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

  def concat[A](lists : List[List[A]]) : List[A] =
    lists.foldLeft(List[A]())((list, acc) => list.foldRight(acc)((elem, acc) => elem :: acc))

  def addOne(list : List[Int]) : List[Int] =
    list.foldRight(List[Int]())((elem, acc) => elem + 1 :: acc)

  def toString(list : List[Int]) : List[String] =
    list.foldRight(List[String]())((elem, acc) => elem.toString() :: acc)

  def map[A, B](as : List[A])(f : A => B) : List[B] =
    as.foldRight(List[B]())((elem, acc) => f(elem) :: acc)

  def filter[A](as : List[A])(f : A => Boolean) : List[A] =
    as.foldRight(List[A]())((elem, acc) => if (f(elem)) elem :: acc else acc)

  def flatMap[A, B](as : List[A])(f : A => List[B]) : List[B] =
    as.foldRight(List[B]())((elem, acc) => f(elem) ++ acc)

  def hasSubsequence[A](sup : List[A], sub : List[A]) : Boolean =
    if (sub.length > sup.length) false
    else (sup, sub) match {
      case (supH :: supT, subH :: subT) => if (supH == subH) hasSubsequence(supT, subT) else hasSubsequence(supT, sub)
      case (Nil, subH :: subT) => false
      case (_, Nil) => true
    }
}