import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created by eyouyan on 8/31/15.
 */
object ConnectedCell {

  /*
  *
  * The class is used to calculate connection in a table with @width and @height
  *
  * the initWeight should be index from 0 until @width * height,
  * the value can be 0 which means not connected on the cell,
  * or positive if it's connected
  * */
  class QuickUnion(val rows: Int, val cols: Int, val initWeight: mutable.Buffer[Int]) {

    private val tableSize = rows * cols

    // an array to donate the roots
    // it point to itself, which means root is it's self
    private val items = new Array[Int](tableSize)
    items.indices.foreach(x => {
      items(x) = x
    })

    private val weights = initWeight

    // calculate when every initial value is set
    calculate()

    // get neighbors of a given cell
    private def neighbors(cell: Int): Array[(Int, Int)] = {
      neighbors(cell / cols, cell % cols)
    }

    // get neighbors of a given cell (row, col)
    private def neighbors(row: Int, col: Int): Array[(Int, Int)] = {
      // assume the give cell is in the middle of the table
      val possibles = Array(
        (row - 1, col - 1), (row - 1, col), (row - 1, col + 1),
        (row, col - 1), /*(row, col),*/ (row, col + 1),
        (row + 1, col - 1), (row + 1, col), (row + 1, col + 1)
      )

      // filter out un-valid neighbors which is outside of the table
      possibles.filter { x => (x._1 >= 0 && x._1 < rows) && (x._2 >= 0 && x._2 < cols) }

    }

    // get root of the given cell
    private def root(cell: Int): Int = {
      val item = items(cell)
      if (item == cell) item
      else root(item)
    }

    //    private def index(row: Int, col: Int) = row * width + col

    private def index(cell: (Int, Int)) = cell._1 * cols + cell._2

    def calculate(): Unit = {
      // for all cells with weight > 0
      val marked = weights.zipWithIndex.filter(_._1 > 0)

      for ((w, index) <- marked) {
        union(index)
//        println(index / cols, index % cols)
//        println(toString)
//        println()
      }
    }

    private def union(cell: Int): Unit = {
      // go through all neighbors
      for (neighbor <- neighbors(cell)) {
        // get root for the given cell, and the neighbor
        // note: the given cell's root could change in the loop as well
        val root1 = root(cell)
        val root2 = root(index(neighbor))
        if (root1 != root2) {
          val w1 = weights(root1)
          val w2 = weights(root2)
          if (w1 > 0 && w2 > 0) {
            if (w1 > w2) {
              items(root2) = root1
              weights(root1) = w1 + w2
            }
            else {
              items(root1) = root2
              weights(root2) = w1 + w2
            }
//            println(toString)
          }
        }
      }
    }

    def maxWeight = max._1

    // get the max weight and max weight root
    def max = weights.zipWithIndex.maxBy(_._1)


//    override def toString: String = {
//      val (maxweight, maxroot) = max
//      val situation = items.zipWithIndex.map {
//        case (item, index) => (root(index), weights(index)).toString()
////          if (root(index) == maxroot) 'X'
////          else if (weights(index) > 0) '1'
////          else '0'
//      }
//      situation.grouped(cols).map(x => x.mkString("\t")).mkString("\n")
//    }
    // get all connected cells for the max weight
    def maxConnected = {
      val (maxweight, maxroot) = max

      // get all cells which has the max weight root
//      items.zipWithIndex.filter { case (item, index) => root(index) == maxroot }.map { case (item, index) => index }

      items.zipWithIndex.map { case (item, index) => if (root(index) == maxroot) 'X' else '0'}

    }
  }

  def main(args: Array[String]): Unit = {
        val rows = io.StdIn.readInt()
        val cols = io.StdIn.readInt()

        // for each row, get the line, split, map toInt, flatten all rows
        val items = (0 until rows).map(x => io.StdIn.readLine().split(' ').map(_.toInt)).flatten.toBuffer

        val union = new QuickUnion(rows, cols, items)
    println(union.maxWeight)
  }
//    // result suppose to be 9
//    val union = new QuickUnion(7, 5,
//      Array(
//        "1 1 1 0 1",
//        "0 0 1 0 0",
//        "1 1 0 1 0",
//        "0 1 1 0 0",
//        "0 0 0 0 0",
//        "0 1 0 0 0",
//        "0 0 1 1 0").map(_.split(' ').map(_.toInt)).flatten.toBuffer)
//    println(union.maxWeight)
//    println(union)
//
//  }

  //    val initWeight = Array(
  //      1,1,0,0,
  //      0,1,1,0,
  //      0,0,1,0,
  //      1,0,0,0)
  //    val union = new QuickUnion(4, 4, initWeight)
  //
  //
  //    union.calculate()
  //    val max = union.maxWeight
  //    println(max)
  //
  //    println(union.maxConnected.mkString(" "))


  //    1 1 0 0
  //    0 1 1 0
  //    0 0 1 0
  //    1 0 0 0

}
