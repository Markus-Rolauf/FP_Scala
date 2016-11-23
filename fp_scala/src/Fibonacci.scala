import scala.annotation.tailrec

object Fibonacci {
//TODO
  def main(args : Array[String]) : Unit = {
    println(fibonacci(1))
    println(fibonacci(2))
    println(fibonacci(3))
    println(fibonacci(4))

    println(System.lineSeparator())
    println(fibonacci(1))
    println(fibonacci(2))
    println(fibonacci(3))
    println(fibonacci(4))
    println(fibonacci(5))
    println(fibonacci(6))
    println(fibonacci(7))

  }

  private def fibonacci(n : Int) : Int = {

    if (n < 1) 0
    else if (n == 1) 1
    else if (n == 2) 1
    else fibonacci(n - 1) + fibonacci(n - 2)

  }

  private def fib(n : Int) : Int = {
      @tailrec
      def fibHelp(n : Int, a : Int, b : Int) : Int = {
        if (n > 0) fibHelp(n - 1, b, a + b)
        else a
      }
    fibHelp(n, 0, 1)
  }

}
