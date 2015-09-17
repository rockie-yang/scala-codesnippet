/**
 * Created by eyouyan on 8/28/15.
 */
object IceCreamParlor {
  var M: Int = 0
  var N: Int = 0
  def solve(left: Int, leftIndex: Int, rightIndex: Int, datas: Array[Int]): String = {
    if (left + datas(rightIndex) == M) {
      (leftIndex + 1).toString + " " + (rightIndex + 1).toString
    }
    else if (rightIndex >= N) {
      solve(datas(leftIndex + 1), leftIndex + 1, leftIndex + 2, datas)
    }
    else {
      solve(left, leftIndex, rightIndex + 1, datas)
    }

  }
  def main(args: Array[String]) {
    val nTestCase = scala.io.StdIn.readInt()
    for (i <- 0 until nTestCase) {
      M = scala.io.StdIn.readInt()
      N = scala.io.StdIn.readInt() - 1
      val datas = scala.io.StdIn.readLine().split(' ').map(_.toInt)
      val init = datas(0)
      println(solve(init, 0, 1, datas))
    }
  }
}
