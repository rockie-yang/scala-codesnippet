


/**
 * Created by eyouyan on 9/8/15.
 */
object Solution {
  import scala.collection.mutable
  import scala.io.Source


  /**
   * XConsole is designed to support rightness check
   *
   * if @name is empty, then it just output using println
   * if @name is not empty, it is a resource file, the resource contains the expected output
   *    then when every time out function is called, it will check if the output is as expected
   */
  class XConsole(expectedResourceName: String = null) {
    var it = if (expectedResourceName == null) null else Source.fromURL(getClass.getResource(expectedResourceName)).getLines().toArray.iterator

    def out(x: Any) = {
      if (it != null) {
        val expected = it.next

        if (x != expected) {
          println(f"expected $expected")
          println(f"actual   $x")
          //          assert(false)
        }
      }
      println(x)
    }
  }

  /**
   * given an @intputResourceName the application read input from that resource
   */
  def solution(inputResourceName: String, console: XConsole): Long = {
    val start = System.currentTimeMillis()

    val input = Source.fromURL(getClass.getResource(inputResourceName))
    solution(input, console)

    System.currentTimeMillis() - start
  }

  /**
   * read input from @inputSource and print out the result to console
   *
   * stdin console can be fetched using <code>Source.fromInputStream(System.in)</code>
   */
  def solution(inputSource: Source, console: XConsole): Unit = {
    val it = inputSource.getLines.toList.iterator
    //    val x = it.toList
    val n = it.next.toInt


    val connection = (1 to n).map(i => it.next.split(' ').map(_.toInt))

    val t = it.next.toInt
    val ks = (0 until t).map(_ => it.next().toInt)

    solve(n, connection, ks, console)
  }

  case class Node(val id: Int, var left: Int, var right: Int, var depth: Int = 0)

  object EmptyNode extends Node(-1, -1, -1, 0)

  def solve(n: Int, data: IndexedSeq[Array[Int]], swaps: IndexedSeq[Int], console: XConsole) = {
    None
  }

  def main(args: Array[String]) {
    //    println(solution("SwapNodeInput1.txt", new XConsole("SwapNodeOutput1.txt")))
    //    println(solution("SwapNodeInput2.txt", new XConsole("SwapNodeOutput2.txt")))
//    solution(Source.fromInputStream(System.in), new XConsole())
    println(solution(Array(-1, 3, -4, 5, 1, -6, 2, 1)))
  }

  def solution(A: Array[Int]): Int = {
    val left = A.scanLeft(0)(_ + _).drop(1)
    val right = A.scanRight(0)(_ + _).dropRight(1)

    var i = 0
    while (i < A.length && left(i) != right(i)) i += 1

    if (i == A.length) -1
    else i
  }
}
