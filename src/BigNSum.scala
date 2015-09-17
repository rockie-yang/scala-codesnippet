/**
 * Created by eyouyan on 8/26/15.
 */
object BigNSum {
  def main(args: Array[String]) {
    val num = io.StdIn.readInt()
    var sum:Long = 0
    for (i <- 0 until num)
      sum += io.StdIn.readInt()
    println(sum)
  }

}
