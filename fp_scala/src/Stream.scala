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

  def foldRight[B](z : => B)(f : (A, => B) => B) : B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def forAll(p : A => Boolean) : Boolean =
    this.foldRight(true)((a, b) => p(a) && b)

  def takeWhile2(pred : A => Boolean) : Stream[A] =
    this.foldRight(empty[A])((a, b) => if (pred(a)) cons(a, b) else empty[A])

  def headOption : Option[A] =
    this.foldRight(None : Option[A])((a, _) => Some(a))

  def filter(pred : A => Boolean) : Stream[A] =
    this.foldRight(empty[A])((h, acc) => if (pred(h)) cons(h, acc) else acc)

  def append[B >: A](s : Stream[B]) : Stream[B] =
    this.foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f : A => Stream[B]) : Stream[B] =
    this.foldRight(empty[B])((h, acc) => f(h) append acc)

  def exists(p : A => Boolean) : Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def unfold[A, S](z : S)(f : S => Option[(A, S)]) : Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => Empty
    }

  def map[B](f : A => B) : Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case empty => None

    }

  def take(n : Long) : Stream[A] =
    unfold(this) {
      case Cons(h, t) => if (n > 1) Some(h(), t()) else None
      case empty => None
    }

  def startsWith[A](s : Stream[A]) : Boolean =
    (this, s) match {
      case (Cons(h1, t1), Cons(h2, t2)) => if (h1() == h2()) t1().startsWith(t2()) else false
      case (empty, _) => true
    }

  def tails : Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    }

  def hasSubsequence[A](s : Stream[A]) : Boolean =
    tails exists { _ startsWith s }

  def scanRight[B](z : B)(f : (A, => B) => B) : Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2

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

  def constant[A](a : A) : Stream[A] =
    cons(a, constant(a))

  def from(n : Int) : Stream[Int] =
    cons(n, from(n + 1))

  def fibs() : Stream[Int] = {
    def go(f1 : Int, f2 : Int) : Stream[Int] =
      cons(f1, go(f1, f1 + f2))
    go(0, 1)
  }

}
