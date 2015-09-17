/**
 * Created by eyouyan on 9/4/15.
 */
object GridWalkingRef {

    var invList = new Array[Long](300);
    var combs = new Array[Array[Long]](301);

    def calInv(a : Long, g : Long) : Long = {
      var divident = g;
      var divisor = a;
      var quotient = divident / divisor;
      var remainder = divident % divisor;
      var oldi = 1L;
      var oldj = 0L;
      var i = -quotient;
      var j = 1L;
      while (remainder != 1) {
        divident = divisor;
        divisor = remainder;
        quotient = divident / divisor;
        remainder = divident % divisor;
        var newi = oldi - quotient * i;
        var newj = oldj - quotient * j;
        oldi = i;
        oldj = j;
        i = newi;
        j = newj;
      }
      if (i < 0) {
        i += g;
      }
      return i;
    }

    def get1DWays(D : Int, x : Int, M : Int) : Array[Long] = {
      var ways = new Array[Long](M + 1);
      ways(0) = 1;
      var arr = new Array[Long](D + 2);
      arr(x) = 1;
      for (i <- 1 to M) {
        var arr1 = new Array[Long](D + 2);
        var sum : Long = 0;
        for (j <- 1 to D) {
          arr1(j) = (arr(j - 1) + arr(j + 1)) % 1000000007;
          sum = (sum + arr1(j)) % 1000000007;
        }
        arr = arr1;
        ways(i) = sum;
      }
      return ways;
    }

    def getCombinations(n : Int, r : Int) : Long = {
      var comb : Long = 1;
      var nn = n;
      var rr = r;
      while (rr > 0) {
        comb = (comb * nn) % 1000000007;
        comb = (comb * invList(rr - 1)) % 1000000007;
        nn -= 1;
        rr -= 1;
      }
      return comb;
    }

    def countPaths() {
      val st = Console.readLine.split(" ");
      val N = st(0).toInt;
      val M = st(1).toInt;
      val x = Console.readLine.split(" ");
      val D = Console.readLine.split(" ");
      var sol = new Array[Long](M + 1);
      sol(0) = 1;
      for (i <- 0 to N - 1) {
        var newsol = new Array[Long](M + 1);
        var newDim = get1DWays(D(i).toInt, x(i).toInt, M);
        for (j <- 0 to M) {
          var jsteps : Long = 0;
          for (k <- 0 to j) {
            newsol(j) = (newsol(j) + (((sol(k) * newDim(j - k)) % 1000000007) * combs(j)(k)) % 1000000007) % 1000000007;
          }
        }
        sol = newsol;
      }
      println(sol(M));
    }

    def populateInv() {
      invList(0) = 1;
      for (i <- 2 to 300) {
        invList(i - 1) = calInv(i.toLong, 1000000007);
      }
    }

    def popCombs() {
      for (i <- 0 to 300) {
        combs(i) = new Array[Long](i + 1);
        for (j <- 0 to i) {
          combs(i)(j) = getCombinations(i, j);
        }
      }
    }

    def main(args: Array[String]) {
      val T = Console.readLine.toInt;
      populateInv();
      popCombs();
      for (i <- 1 to T) {
        countPaths();
      }
    }

}
