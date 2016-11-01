import scala.annotation.tailrec

object Sorted {
  def main(args : Array[String]) : Unit = {

    def arr = Array(0, 1, 2, 3, 4, 5, 6)
    println(isSorted[Int](arr, (a, b) => a < b))

    def arr2 = Array(0, 1, 2, 4, 3, 5, 6)
    println(isSorted[Int](arr2, (a, b) => a < b))
  }

  def isSorted[A](asArray : Array[A], comp : (A, A) => Boolean) : Boolean = {
    @tailrec
    def loop(n : Int) : Boolean = {
      if (n > asArray.size - 2) true
      else if (!comp(asArray(n), asArray(n + 1))) false
      else loop(n + 1)
    }
    loop(0)
  }
}