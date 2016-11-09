import Stream._

trait Stream[+A] {

  def take(n : Int) : Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), Empty)
    case _ => Empty
  }

  def drop(n : Int) : Stream[A] = this match {
    case Cons(h, t) if n > 1 => t().drop(n - 1)
    case Cons(h, t) if n == 1 => t()
    case Empty => Empty
  }

  def takeWhile(pred : A => Boolean) : Stream[A] =
    this match {
      case Cons(h, t) => if (pred(h())) cons(h(), t().takeWhile(pred)) else Empty
      case Empty => empty
    }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h : () => A, t : () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd : => A, tl : => Stream[A]) : Stream[A] = {
    lazy val head = hd
    lazy val tail = tl

    Cons(() => head, () => tail)
  }

  def empty[A] : Stream[A] = Empty

  def apply[A](as : A*) : Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail : _*))
}
