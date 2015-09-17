/**
 * Created by eyouyan on 8/28/15.
 */
object SherlokNArray {
  // it work for all items
  def solution(A: Array[Int]): Int = {
    val long = A.map(_.toLong)
    val left = long.scanLeft(0L)(_ + _).drop(1)
    val right = long.scanRight(0L)(_ + _).dropRight(1)

    var i = 0
    while (i < A.length && left(i) != right(i)) i += 1

    if (i == A.length) -1
    else i
  }

  // only works if item >= 0
  def solve(leftSum: Long, rightSum: Long, leftIndex: Int, rightIndex: Int, datas: Array[String]): String = {
    if ((rightIndex - leftIndex) <= 1) {
      if (leftSum == rightSum) "YES" else "NO"
    }
    else {
      if (leftSum < rightSum) {
        solve(leftSum + datas(leftIndex).toInt, rightSum, leftIndex + 1, rightIndex, datas);
      }
      else {
        solve(leftSum, rightSum + datas(rightIndex).toInt, leftIndex, rightIndex - 1, datas);
      }
    }
  }
  def main(args: Array[String]) {
    val nTestCase = scala.io.StdIn.readInt()
    for (i <- 0 until nTestCase) {
      val num = scala.io.StdIn.readInt()
      val datas = scala.io.StdIn.readLine().split(' ')
      println(solve(0, 0, 0, datas.length - 1, datas))
    }
  }
}
