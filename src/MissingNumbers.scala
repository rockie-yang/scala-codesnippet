/**
 * Created by eyouyan on 8/28/15.
 */
object MissingNumbers {
  def main(args: Array[String]): Unit = {

    val n = io.StdIn.readInt()
    val ns = io.StdIn.readLine().split(' ').map(_.toInt).groupBy(x => x).map{case (k, v) => (k, v.length)}

    val m = io.StdIn.readInt()
    val ms = io.StdIn.readLine().split(' ').map(_.toInt).groupBy(x => x).map{case (k, v) => (k, v.length)}


    val diff = (ns.toSet diff ms.toSet) map {case (k, v) => k} toList

    println(diff.sorted.mkString(" "))
  }
}
