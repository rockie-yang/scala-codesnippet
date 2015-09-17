/**
 * Created by eyouyan on 9/15/15.
 */
object MaximizeStockEarn {

  def solve(prices: IndexedSeq[Int]): Long = {
    val n = prices.length

    // if there is less than a day, then it could not have any profit
    if (n <= 1) {
      0
    }
    else {
      // 1 for buy, 0 for sell
      val transaction = new Array[Char](n)

      var profit = 0L
      var highest_price = 0

      // loop from the last day to the first day
      // buy at any day [i, n) which the prices is lower then the highest price max(i, n)

      // the number of share increases until reach a day with highest price
      // from the days buy and to the last day
      //
      // day  0   1   2   3
      //price 1   3   1   2
      //tran  b   s   b   s
      //share 1   0   1   0
      //                  2 is the highest, and profit is 2 - 2 = 0
      //              2 is the highest, and profit is 2 - 1 = 1
      //          3 is the highest price and profit is 3 - 3 = 0,
      //                        and it could not affect later days, since all shares have been sold
      //      3 is the highest and profit is 3 - 1 = 2
      for (i <- n - 1 to 0 by -1) {
        val price = prices(i)

        if (highest_price < price) {
          highest_price = price
          transaction(i) = 's'
        }
        else {
          transaction(i) = 'b'
        }

        profit += highest_price - price
      }

//      println(prices.mkString("\t"))
//      println(transaction.mkString("\t"))

      val shares = transaction.scanLeft(0){
        (res, t) =>
          // number of shares increases until we sell it
          // and it re-accumulate again
          if (t == 's') 0 else res + 1
      }
//      println(shares.tail.mkString("\t"))
      profit
    }
  }

  def main(args: Array[String]): Unit = {
//    assert(solve(Array(5, 3, 2)) == 0)
//    assert(solve(Array(1, 2, 100)) == 197)
//    assert(solve(Array(1, 3, 1, 2)) == 3)
//    assert(solve(Array(1, 1, 3, 2)) == 4)
    val nTestCase = io.StdIn.readInt()
    for (_ <- 0 to nTestCase) {
      val days = io.StdIn.readInt()
      val prices = io.StdIn.readLine().split(' ').map(_.toInt)
      println(solve(prices))
    }
  }
}
