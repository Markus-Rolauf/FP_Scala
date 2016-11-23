
object Functions {

  def curry[A, B, C](f : (A, B) => C) : A => (B => C) =
    a => (b => f(a, b))

  def uncurry[A, B, C](f : A => B => C) : (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A, B, C](f : A => B, g : B => C) : A => C =
    a => g(f(a))
}