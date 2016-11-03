import scala.collection.immutable.Stream.Cons
import scala.collection.immutable.Nil

object Lists {
  def main(args : Array[String]) : Unit = {

    val x = List(1, 2, 3, 4, 5, 6) match {
      case x :: (2 :: (4 :: _)) => x
      case Nil => 42
      case x :: y :: 3 :: 4 :: _ => y
      case h :: t => 11
      case _ => 101
    }

    println(x)
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

}