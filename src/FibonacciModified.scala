/**
 * Created by eyouyan on 9/7/15.
 */
object FibonacciModified {

  def eval(a: Int, b: Int, n: Int): BigInt = {

    n match {
      case 0 => a
      case 1 => b
      case _ =>
        var a_ = BigInt(a)
        var b_ = BigInt(b)
        for (_ <- 2 until n) {
          val tmp = b_ * b_ + a_
          a_ = b_
          b_ = tmp
        }

        b_
    }
  }

  def main(args: Array[String]) {
    val a::b::n::Nil = io.StdIn.readLine().split(' ').map(_.toInt).toList
//    assert(eval(0, 1, 10) == 5)
//    assert(eval(0, 1, 10) == BigInt("84266613096281243382112"))
    println(eval(a, b, n))

  }
}
