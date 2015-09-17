
import scala.collection.mutable
import scala.io.Source

/**
 * Created by eyouyan on 9/8/15.
 */
object SwapNodes {

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

  def solve(n: Int, data: IndexedSeq[Array[Int]], swaps: IndexedSeq[Int], console: XConsole): mutable.ArrayBuffer[Int] = {
    // the node is from 1 to n, so we allocated n+1, just leave the 0 element unused
    val nodes = new Array[Node](n + 1)

    // construct the tree from bottom up
    // since it is guaranteed that index of child if always greater than of parent
    data.zipWithIndex.reverse.map {
      case (ar, i) =>
        val leftIndex = ar(0)
        val rightIndex = ar(1)
        // index from 1, so we need i+1
        nodes(i + 1) = Node(i + 1, leftIndex, rightIndex)
    }

    // it should be always NoneEmptyNode
    nodes(1) match {
      case node: Node => node.depth = 1
    }

    // store all element in the same level to a set
    val levels = mutable.Map[Int, mutable.Set[Int]]()

    def setDepth(): Unit = {
      val queue = mutable.Queue[Int]()
      queue.enqueue(1)

      while (!queue.isEmpty) {
        val nodeId = queue.dequeue()
        val node = nodes(nodeId)

        if (levels.contains(node.depth)) {
          levels(node.depth) += nodeId
        }else {
          levels(node.depth) = mutable.Set(nodeId)
        }
        node.left match {
          case -1 => // do nothing
          case id =>
            nodes(id).depth = node.depth + 1
            queue.enqueue(id)
        }

        node.right match {
          case -1 => // do nothing
          case id =>
            nodes(id).depth = node.depth + 1
            queue.enqueue(id)
        }
      }
    }

    setDepth()

    val totalLevel = levels.keys.max

    // for each specified swap
    swaps.foreach {
      swap =>
        // all levels [swap, 2swap, 3swap ...] need be swapped
        val swapLevels = (1 to totalLevel).filter { level => level % swap == 0 }

        swapLevels.foreach {
          level =>
            if (levels.contains(level)) {
              // for all node at level which need be swapped
              for (id <- levels(level)) {
                val node = nodes(id)
                val tmp = node.left
                node.left = node.right
                node.right = tmp
              }
            }
        }
        val inorder = inOrder()
        console.out(inorder.mkString(" "))
    }

    def inOrder() = {
      val order = mutable.ArrayBuffer[Int]()
      def inOrder0(id: Int) {
        if (id != -1) {
          val node = nodes(id)

          inOrder0(node.left)

          order.append(id)

          inOrder0(node.right)
        }
      }
      inOrder0(1)
      order
    }

    inOrder
  }

  def main(args: Array[String]) {
//    println(solution("SwapNodeInput1.txt", new XConsole("SwapNodeOutput1.txt")))
//    println(solution("SwapNodeInput2.txt", new XConsole("SwapNodeOutput2.txt")))
    solution(Source.fromInputStream(System.in), new XConsole())
  }
}
