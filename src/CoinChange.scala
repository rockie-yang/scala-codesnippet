import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


/**
 * Created by eyouyan on 9/8/15.
 */
object CoinChange {
  // http://www.geeksforgeeks.org/dynamic-programming-set-7-coin-change/
  def solveWith2DMemoize(coins: Array[Int], amount: Int): Long = {
    val cs = coins.size
    // it's a two dimension table
    // the first  dimension is amount money
    // the second dimension is coin index
    //
    // for a given amount of money
    // the second dimension means how many solution if using coins[0:j]
    val table = new Array[Array[Long]](amount + 1)
    table.indices.foreach{i =>
      table(i) = new Array[Long](cs)
      table(i).indices.foreach{
        j => table(i)(j) = 0L
      }
    }

    // for amount of money 0, use any coin have 1 solution which is using 0 of coin j
    table(0).indices.foreach {
      j => table(0)(j) = 1L
    }

    // it's a bottom up solution, first count if amount is 1, how many solutions there are
    // if using coins[0:j] for all j in [0, cs]
    for (i <- 1 to amount) {
      for (j <- 0 until cs) {
        // how many solution there is if we using coin j
        val coin = coins(j)
        val left = i - coin
        // table(left) already calculated when previous i == left
        // and in this loop i > left
        val x = if (left >= 0) table(left)(j) else 0

        // how many solution there is if we don't using coin j
        // table(i)(j-1) just calculated in previous step
        val y = if (j >= 1) table(i)(j - 1) else 0

        // total solution will be sum of using coin j and not using coin j
        table(i)(j) = x + y
      }
    }

    val solutions = table(amount)(cs - 1)
    //    println(f"there are $solutions solutions")
    solutions
  }

  def solveWith1DMemoize(coins: Array[Int], amount: Int): Long = {
    val n = coins.length
    val table = new Array[Long](amount + 1)
    table.indices.foreach(i => table(i) = 0L)
    table(0) = 1L // change for 0 have one solution which is no coins

    //    println("  : " + table.indices.mkString("\t"))

    for (i <- 0 until n) { // how many solution existed if using coin[0, i]

      // for j < coins(i), using coins(i) have no extra solutions added
      for (j <- coins(i) to amount) {
        // the solution existed for amount(j - coins(i)) add one more coins(j)
        // will be a solution for amount j
        // the previous solution for amount j using coins(0:i -1) is already stored in table(j)
        // add the number of solution if could using one more coin j
        table(j) += table(j - coins(i))
        //        println(j.toString + " : " + table.mkString("\t"))
      }

      //      println(coins(i).toString + " : " + table.mkString("\t"))
    }
    val solutions = table(amount)
    //    println(f"total solution $solutions")
    solutions
  }

  def solveWith1DMemoizeWithSolutions(coins: Array[Int], amount: Int): List[Array[Int]] = {
    val n = coins.length
    val table = new Array[List[Array[Int]]](amount + 1)
    table.indices.foreach(i => table(i) = Nil)

    val zeroCoin = new Array[Int](n)
    zeroCoin.indices.foreach(i => zeroCoin(i) = 0)
    table(0) = List(zeroCoin) // change for 0 have one solution which is no coins

    //    println("  : " + table.indices.mkString("\t"))

    for (i <- 0 until n) { // how many solution existed if using coin[0, i]
      val coin = coins(i)
      // for j < coins(i), using coins(i) have no extra solutions added
      for (j <- coins(i) to amount) {
        // the solution existed for amount(j - coins(i)) add one more coins(j)
        // will be a solution for amount j
        // the previous solution for amount j using coins(0:i -1) is already stored in table(j)
        // add the number of solution if could using one more coin j
        val existingSolutionsWithoutUsingCoin_i = table(j)
        val newSolutionsUsing1ExtraCoin_i = table(j - coins(i)).map {
          changes =>
            val previousNumCoin_i = changes(i)
            changes.updated(i, previousNumCoin_i + 1)
        }
        table(j) = existingSolutionsWithoutUsingCoin_i ::: newSolutionsUsing1ExtraCoin_i
        //        println(j.toString + " : " + table.mkString("\t"))
      }

      //      println(coins(i).toString + " : " + table.mkString("\t"))
    }
    println(coins.mkString("\t"))
    val solutions = table(amount)
    solutions.foreach {
      solution => println(solution.mkString("\t"))

    }
    //    println(f"total solution $solutions")
    solutions
  }

  def main(args: Array[String]): Unit = {
    //    assert(solve0(Array(1, 2, 5), 10) == 10)
//    assert(solveWith1DMemoizeWithSolutions(Array(1, 2, 5), 10).length == 10)
    //    assert(solve(Array(1, 2, 5), 10) == 4)
    //
    //      assert(solve(Array(1, 2, 3), 4) == 4)
    //      assert(solve(Array(2, 3, 5, 6), 10) == 5)

    //    assert(solve0(Array(41,34,46,9,37,32,42,21,7,13,1,24,3,43,2,23,8,45,19,30,29,18,35,11).sorted, 250) == 15685693751L)
    //    assert(solve0(Array(44,5,9,39,6,25,3,28,16,19,4,49,40,22,2,12,45,33,23,42,34,15,46,26,13,31,8).sorted, 2) == 1)
    val amount :: cs :: Nil = io.StdIn.readLine().split(' ').map(_.toInt).toList
    val coins = io.StdIn.readLine().split(' ').map(_.toInt)

    val solution = solveWith2DMemoize(coins, amount)
    println(solution)
  }
}
