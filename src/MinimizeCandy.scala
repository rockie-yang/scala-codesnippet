/**
 * Created by eyouyan on 9/15/15.
 */
object MinimizeCandy {
  def solve(rates: IndexedSeq[Int]) = {

    // from child 0 to i, what is the minimum candies needed
    val candies = new Array[Int](rates.length)
//    table.indices.foreach(i => table(i) )

    candies(0) = 1 // if only one child, then we need 1 candy

    var prevRate = rates(0)
    var prevCandy = 1

    for (i <- 1 until rates.length) {
      rates(i) match {
        case x if x == prevRate => // candy can be decreased to 1
          prevCandy = 1
          candies(i) = prevCandy
        case x if x > prevRate => // one more candy needed
          prevCandy += 1
          candies(i) = prevCandy
        case x if x < prevRate => // less candy needed
          prevCandy = 1
          candies(i) = prevCandy
          if (prevCandy <= 1) {  // we can not decrease candy, back trace is needed
            var currRate = rates(i)
            var currCandy = candies(i)
            var j = i - 1
            // rates(x) > rates(x+1) but the candies(x) <= candies(x+1)
            while (j >= 0 && rates(j) > currRate && candies(j) <= currCandy) {
              candies(j) += 1
              currRate = rates(j)
              currCandy = candies(j)
              j -= 1
            }
          }

      }

      prevRate = rates(i)
    }
//    println("minimum candy needed: " + candies.sum)
//    println(rates.mkString("\t"))
//    println(candies.mkString("\t"))

    candies.sum
  }

  def main(args: Array[String]): Unit = {
//    assert(solve(Array(1,2,2)) == 4)
//    assert(solve(Array(2,4,2,6,1,7,8,9,2,1)) == 19)

    val n = io.StdIn.readInt()
    val rates = (0 until n).map{
      i => io.StdIn.readInt()
    }
    println(solve(rates))
  }
}
