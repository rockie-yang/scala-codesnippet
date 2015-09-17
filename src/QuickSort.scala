
/**
 * Created by eyouyan on 9/1/15.
 */
object QuickSort {
//  def sort(xs: Array[Int]) {
//    def swap(i: Int, j: Int) {
//      val t = xs(i); xs(i) = xs(j); xs(j) = t
//    }
//    def sort1(l: Int, r: Int) {
//      val pivot = xs((l + r) / 2)
//      var i = l; var j = r
//      while (i <= j) {
//        while (xs(i) < pivot) i += 1
//        while (xs(j) > pivot) j -= 1
//        if (i <= j) {
//          swap(i, j)
//          i += 1
//          j -= 1
//        }
//      }
//      if (l < j) sort1(l, j)
//      if (j < r) sort1(i, r)
//    }
//    sort1(0, xs.length - 1)
//  }

  def sort[T](a: Array[T])(implicit orderer: T => Ordered[T]) = {
    def swap(i: Int, j: Int): Unit = {
      if (i != j) {
        val swp = a(i)
        a(i) = a(j)
        a(j) = swp
      }
    }

    def shuffle: Unit = {
      val len = a.length
      for (i <- 0 until a.length) {
        val r = i + (math.random * (len - i)).toInt
        swap(i, r)
      }
    }


    // Lomuto partition scheme
    def partition(lo: Int, hi: Int): Int = {
      val pivot = a(hi)
      var i = lo

      for (j <- lo until hi) {
        if (a(j) <= pivot) {
          swap(i, j)
          i += 1
        }
      }
      swap(i, hi)
      println(a.mkString(" "))
      i
    }

    def sort0(lo: Int, hi: Int): Unit = {

      if (lo < hi) {
        val p = partition(lo, hi)
        sort0(lo, p - 1)
        sort0(p + 1, hi)
      }

    }

//    shuffle
//    println(a.mkString(" "))
    sort0(0, a.length - 1)
  }

  def main(args: Array[String]): Unit = {
//    runTest()
    val n = io.StdIn.readInt()
    sort(io.StdIn.readLine().split(' ').map(_.toInt))

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

  def runTest(): Unit = {
//    val data = sort("1 3 9 8 2 7 5".split(' ').map(_.toInt))
//    assertEqual(data.mkString(" "), "1 2 3 5 7 8 9")
////    val datax = sort("1 7 2 3 5 9 8".split(' ').map(_.toInt).toArray)
//    val data2 = sort("1 7 2 3 5 9 8".split(' ').map(_.toInt))
//    assertEqual(data2.mkString(" "), "1 2 3 5 7 8 9")
  }
}
