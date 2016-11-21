import State._

case class State[S, +A](run : S => (A, S)) {

  def flatMap[B](f : A => State[S, B]) : State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })

  def map[B](f : A => B) : State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B, C](sb : State[S, B])(f : (A, B) => C) : State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))
}
object State {

  def unit[S, A](a : A) : State[S, A] =
    State(s => (a, s))

  def sequence[S, A](sas : List[State[S, A]]) : State[S, List[A]] = {
    def go(s : S, actions : List[State[S, A]], acc : List[A]) : (List[A], S) =
      actions match {
        case Nil => (acc.reverse, s)
        case h :: t => h.run(s) match { case (a, s2) => go(s2, t, a :: acc) }
      }
    State((s : S) => go(s, sas, List()))
  }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked : Boolean, candies : Int, coins : Int)