/**
 * Created by eyouyan on 8/26/15.
 */
object SimplePlusIO {
  def main(args: Array[String]) {
    println(io.Source.stdin.getLines().take(2).map(_.toInt).sum)
  }
}
