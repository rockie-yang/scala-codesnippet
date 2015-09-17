import scala.annotation.tailrec

/**
 * Created by eyouyan on 9/3/15.
 */
object GridWalking {


  def solve(ndim: Int, totalSteps: Int, initPositions: Array[Int], dimensions: Array[Int]): Int = {

    val prev = (0 until ndim).map { dim => (dim, -1) }
    val next = (0 until ndim).map { dim => (dim, 1) }

    val offsets = prev ++ next

    var ways = 0

    def solve0(remainSteps: Int, currenctPositions: Array[Int]): Unit = {

      def onBoarder: Boolean = {
        currenctPositions.zip(dimensions).forall {
          case (position, dimension) =>
            position == 0 || position == (dimension - 1)
        }
      }

      def inGrid(position: Int, dim: Int) = {
        position >= 0 && position < dimensions(dim)
      }

      def neighbors = {
        offsets.collect {
          case (dim, offset) if inGrid(currenctPositions(dim) + offset, dim) =>
            currenctPositions.patch(dim, Seq(currenctPositions(dim) + offset), 1)
        }
      }

      def out: Boolean = {
        currenctPositions.zip(dimensions).exists {
          case (position, dimension) => position < 0 || position >= dimension
        }
      }

      if (remainSteps > 0) {
        for (neighbor <- neighbors) {
          solve0(remainSteps - 1, neighbor)
        }
      }
      else {
        ways += 1
        if (ways % 1000 == 0) println(ways)
      }
    }



    solve0(totalSteps, initPositions)
    ways
    //
    //  }

    //  def solve(totalSteps: Int, initPosition: Int, dimension: Int): Int = {
    //
    //    @tailrec
    //    def solve0(remainSteps: Int, currenctPosition: Int): Int = {
    //
    //      //      val onBoarder: Boolean = {
    //      //        currenctPositions.zip(dimensions).forall {
    //      //          case (position, dimension) =>
    //      //            position == 0 || position == (dimension -1)
    //      //        }
    //      //      }
    //      //
    //      //      def inGrid(position: Int, dim: Int) = {
    //      //        position >= 0 && position < dimensions(dim)
    //      //      }
    //      //
    //      //      def neighbors = {
    //      //        offsets.collect {
    //      //          case (dim, offset) if inGrid(currenctPositions(dim) + offset, dim) =>
    //      //            currenctPositions.patch(dim, Seq(currenctPositions(dim) + offset), 1)
    //      //        }
    //      //      }
    //      //
    //      val in: Boolean = {
    //        currenctPosition >= 0 && currenctPosition < dimension
    //      }
    //
    //      if (remainSteps == 0) {
    //        if (in) 1 else 0
    //      }
    //      else {
    //        solve0(remainSteps - 1, currenctPosition + 1) + solve0(remainSteps - 1, currenctPosition - 1)
    //      }
    //    }
    //
    //
    //
    //    solve0(totalSteps, initPosition)
  }


  def main(args: Array[String]): Unit = {
    assertEqual(solve(1, 1, Array(1), Array(2)), 1)
    assertEqual(solve(1, 1, Array(0), Array(2)), 1)

    println(solve(1, 3, Array(0), Array(3)))
    println(solve(1, 3, Array(0), Array(4)))

    assertEqual(solve(1, 287, Array(44), Array(78)), 38753340)
  }

  // simple assert to display more understandable message
  def assertEqual(actual: Any, expected: Any): Unit = {
    if (actual != expected) {
      println(s"expected value $expected, but it was $actual")
      assert(actual == expected)
    }
  }

  // simple assert to display more understandable message
  def assertNotEqual(actual: Any, expected: Any): Unit = {
    if (actual == expected) {
      println(s"not expected value $expected, but it was")
      assert(actual != expected)
    }
  }
}
