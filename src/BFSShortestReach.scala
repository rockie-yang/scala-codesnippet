


/**
 * Created by eyouyan on 9/8/15.
 */
object BFSShortestReach {

  import scala.io.Source
  import scala.collection.mutable

  // the node is from 1 to n
  def solve(n: Int, s: Int, data: IndexedSeq[Array[Int]]): String = {
    // initialize the graph using an Array, each element is an ArrayBuffer
    // the element in ArrayBuffer is the connected node
    val graph = new Array[mutable.Set[Int]](n + 1)
    graph.indices.foreach { i => graph(i) = mutable.Set[Int]() }

    // connect nodes according to given data
    data.foreach { ar =>
      val x = ar(0)
      val y = ar(1)
      graph(x) += y
      graph(y) += x
    }


    def bfs: String = {
      val processed = mutable.Set[Int]()
      // all initial distance is -1
      val distances = new Array[Int](n + 1)
      distances.indices.foreach(i => distances(i) = -1)


      // except for the start node is 0
      distances(s) = 0

      // create a queue for breadth-first-search
      val queue = new mutable.Queue[Int]()
      queue.enqueue(s)

      while (!queue.isEmpty) {
        // the current node
        val x = queue.dequeue()
        // the distance from start node to the neighbor node
        val dx = distances(x) + 6

        //        if (processed.contains(x)) {
        //          println(f"$x already processed, there must be an cyclic loop")
        //        } else {
        //          println(f"process $x")
        //        }

        if (!processed.contains(x)) {
          processed.add(x)

          graph(x).foreach {
            y =>
              // remove the pair connection
              graph(y) -= x
              if (!processed.contains(y))
                queue.enqueue(y)


              val dy = distances(y)
              dy match {
                case -1 =>
                  distances(y) = dx
                case d if dy > dx =>
                  distances(y) = dx
                case _ => // do nothing if the distance is getting bigger from that node
              }

          }
        }
      }

      dsString(distances, s)
    }
    bfs
  }

  // the first element is just a stakeholder
  def dsString(ds: Array[Int], s: Int): String = {
    val filtered = ds.zipWithIndex.collect {
      case (d, i) if i != s => d
    }

    // remove the stakeholder
    filtered.tail.mkString(" ")
  }

  def solution(name: String, console: XConsole): Long = {
    val start = System.currentTimeMillis()

    val input = Source.fromURL(getClass.getResource(name))
    solution(input, console)

    System.currentTimeMillis() - start
  }

  def solution(source: Source, console: XConsole): Unit = {
    val it = source.getLines.toList.iterator
    //    val x = it.toList
    val nTestCase = it.next.toInt


    for (_ <- 0 until nTestCase) {
      val n :: m :: Nil = it.next.split(' ').map(_.toInt).toList

      val connection = (0 until m).map(_ => it.next.split(' ').map(_.toInt))

      val s = it.next.toInt

      console.out(solve(n, s, connection))
    }
  }


  class XConsole(name: String = null) {
    var it = if (name == null) null else Source.fromURL(getClass.getResource(name)).getLines().toArray.iterator

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


  def main(args: Array[String]) {
    //    solution("BFSShortestReachinput00.txt", new XConsole("BFSShortestReachoutput00.txt"))
    //    solution("BFSShortestReachinput01.txt", new XConsole("BFSShortestReachoutput01.txt"))
    //    solution("BFSShortestReachinput02.txt", new XConsole("BFSShortestReachoutput02.txt"))
//    println(solution("BFSShortestReachinput03.txt", new XConsole("BFSShortestReachoutput03.txt")))

    solution(Source.fromInputStream(System.in), new XConsole())
  }
}
