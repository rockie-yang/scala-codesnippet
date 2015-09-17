


/**
 * Created by eyouyan on 9/8/15.
 */
object MaxSum1D {

  import scala.io.Source


  /**
   * XConsole is designed to support rightness check
   *
   * if @name is empty, then it just output using println
   * if @name is not empty, it is a resource file, the resource contains the expected output
   * then when every time out function is called, it will check if the output is as expected
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
    val lines = inputSource.getLines.toList
    val it = lines.iterator
    //    val x = it.toList
    val nTestCase = it.next.toInt

    for (_ <- 0 until nTestCase) {
      val n = it.next.toInt
      val data = it.next.split(' ').map(_.toInt)

      solve(n, data, console)
    }

  }

  case class Node(val id: Int, var left: Int, var right: Int, var depth: Int = 0)

  object EmptyNode extends Node(-1, -1, -1, 0)

  def maxContinues(data: IndexedSeq[Int]): Long = {
    var best = 0
    var cur = 0

    for (d <- data) {
      cur = math.max(cur + d, 0)
      best = math.max(best, cur)
    }

    val maxItem = data.max

    // it is for nonempty subset
    // so at least one item need be included
    if (maxItem < 0) maxItem
    else best
  }

  def maxNoncontinues(data: IndexedSeq[Int]): Long = {
    val postiveSum = data.filter(_ > 0).sum

    val maxItem = data.max
    if (maxItem < 0) maxItem
    else postiveSum
  }

  def solve(n: Int, data: IndexedSeq[Int], console: XConsole) = {
    val m1 = maxContinues(data)
    val m2 = maxNoncontinues(data)

    val output = f"$m1 $m2"
    console.out(output)
  }

  def main(args: Array[String]) {
//    println(solution("MaxSum1DInput0.txt", new XConsole("MaxSum1DOutput0.txt")))
//        println(solution("MaxSum1DInput1.txt", new XConsole("MaxSum1DOutput1.txt")))
    solution(Source.fromInputStream(System.in), new XConsole())
  }
}
