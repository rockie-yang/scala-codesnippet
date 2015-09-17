
/**
 * Created by eyouyan on 9/1/15.
 */
object CountLuck {
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

  // check if it's run in debug mode
  // some information can be printed for debug purpose
  val isDebug = java.lang.management.ManagementFactory.getRuntimeMXBean().
    getInputArguments().toString().indexOf("-agentlib:jdwp") > 0

  class Solver(val rows: Int, val cols: Int, initMap: String) {

    val size = rows * cols
    val exitCell = initMap.indexOf('*')
    val startCell = initMap.indexOf('M')
    var waves = 0
    val map = initMap.toBuffer
    
    // how the forest is visited with the path
    val path = map.map(ch => -1)

    // let's solve it
    solve(startCell)

    // get neighbors of a given cell (row, col)
    private def neighbors(cell: Int): Array[Int] = {
      val (row, col) = (cell / cols, cell % cols)

      // assume the give cell is in the middle of the table
      val possibles = Array(
        (row - 1, col),
        (row, col - 1), /*(row, col),*/ (row, col + 1),
        (row + 1, col))

      // filter out un-valid neighbors which is outside of the table
      possibles.filter { case (row, col) =>
        (row >= 0 && row < rows) &&
          (col >= 0 && col < cols) &&
          ".*".contains(map(row * cols + col))
      }.map { case (row, col) => row * cols + col }
    }

    def impressed(expectedWaves: Int) = if (expectedWaves == waves) "Impressed" else "Oops!"

    // the path has been found, reconstruct the path reversely from exitCell to startCell
    def constructSolution = {
      var cell = exitCell
      var wholePath = List[Int](startCell)

      do{
          wholePath = cell :: wholePath
        cell = path(cell)
      } while (cell != startCell)

      val wavedPath = wholePath.filter{cell => map(cell) == '#'}
      waves = wavedPath.length

      if (isDebug) {
        val showPath = map.indices.map{
          index =>
            if (wavedPath.contains(index)) '#'
            else initMap(index)
        }

        println(showPath.grouped(cols).map(x => x.mkString("")).mkString("\n"))
        println(waves)
      }
    }

    // solve the problem
    // this only works if the only path exist
    // if there are more then one path, then the solution is only one of the solution, may NOT be the best
    def solve(cell: Int): Unit = {
      // the exit has been found
      if (cell == exitCell) {
        constructSolution
      }
      else {
        // get all unvisited neighbors which could go next
        val theneighbors = neighbors(cell)

        // tap it we need wave if pass by here
        if (theneighbors.length > 1) {
          map(cell) = '#'
        }
        // tap it we have visited here
        else {
          map(cell) = '+'
        }

        // traverse all neighbors
        theneighbors.foreach(neighbor => {
          // the path to neighbor is from the cell
          path(neighbor) = cell
          solve(neighbor)
        })
      }

    }

  }

  def main(args: Array[String]): Unit = {
    if (isDebug) {
//      runTest()
    }
    val nTestCase = io.StdIn.readInt()

    for (_ <- 0 until nTestCase) {
      val rows :: cols :: Nil = io.StdIn.readLine().split(' ').map(_.toInt).toList

      val map = (0 until rows).map(x => io.StdIn.readLine()).mkString
      val expectedWaves = io.StdIn.readInt()

      println(new Solver(rows, cols, map).impressed(expectedWaves))
    }

  }

  def runTest(): Unit = {
          assertEqual(new Solver(2, 3, "*.M" + ".X.").waves, 1)

          assertEqual(new Solver(4, 11,
            ".X.X......X" +
              ".X*.X.XXX.X" +
              ".XX.X.XM..." +
              "......XXXX.").waves, 3)

          assertNotEqual(new Solver(37, 31,
            "X.XXXX.XX....X.XX...X.XXXXXXXXX" +
              "X.XXX...XXX.X..XXX.XX..XXXXXXXX" +
              "...X.XX..X...X..XX.X..XXXXXXXXX" +
              "X.X..X..X.X.X..X.....XX.XXXXXXX" +
              "...X..X.X....X.X.X.X.X..XXXXXXX" +
              ".X..X....X.X.....XX....XXXXXXXX" +
              "..X..XX.X.X..XX.X..XX.XXXXXXXXX" +
              ".XXX.X.....X.X.X*.X.XX.XXXXXXXX" +
              "X..X..X.X.X.....X....X..XXXXXXX" +
              "..X.X....X..XXXX..XXX..XXXXXXXX" +
              "X....XXX..X.....X...X.XXXXXXXXX" +
              "..XX.....X.X.X.X..X.X..XXXXXXXX" +
              "XX.X.X.X...X.XX.X..X..X..XXXXXX" +
              ".M...XXXX.X.....X.X.X...XXXXXXX" +
              "X.XXX..X...X.X.X..X..X.XXXXXXXX" +
              ".XX...X.XX..X..X.X.X....XXXXXXX" +
              "....X......X..X......XXX.XXXXXX" +
              "X.X.XX.XXX..X.X.XX.XX.....XXXXX" +
              "X..X..X....XX.....X...X.X..XXXX" +
              "..X.X...XX....X.XX..X.X..XX.XXX" +
              ".X..X.X.X.X.XX...X.X.X.XX...XXX" +
              "XX.X...X....X..X........XX.X.XX" +
              ".....XX..X.X.XX..XX.X.X......XX" +
              "X.X.XX..X.XX....X..XXXXX.XXXX.X" +
              "XX..X.XX....XXX...X.X.X........" +
              ".XXXX...XXX.....X......XX.XX.XX" +
              "......XX....X.X..XX.XX...XX.XXX" +
              "X.X.X....X.X...X...X...X.....X." +
              "XX...X.X..X..X.XXXX.XX.X.X.X..." +
              "XXXX..X..XXX.X......X.X...X..XX" +
              "XXX..XXX..X..XX.X.X....XX..X..X" +
              "X.XX..XX.X..X...X..X.X.XX.X..XX" +
              "...XXXX..X.X..X..X..XXX...XXXXX" +
              "XX...X..XX.XX..X..X.XX..X..XXXX" +
              "XX.XX..XXXXX.X.X.X...X.XXXXXXXX" +
              "X.....X.XXX..X.X..X.XXXXXXXXXXX" +
              "..X.X......X...X.X..XXXXXXXXXXX").waves, 20)

          assertEqual(new Solver(3, 41,
            ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X." +
              "M.......................................*" +
              ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.").waves, 20)

          assertEqual(new Solver(5, 11,
            "..........*" +
              ".XXXXXXXXXX" +
              "..........." +
              "XXXXXXXXXX." +
              "M..........").waves , 0)

          assertNotEqual(new Solver(4, 11,
            ".X.X......X"+
            ".X*.X.XXX.X"+
            ".XX.X.XM..."+
            "......XXXX.").waves, 4)
            assertNotEqual(new Solver(4, 11,
            ".X.X......X"+
            ".X*.X.XXX.X"+
            ".XX.X.XM..."+
            "......XXXX.").waves, 40)

          assertEqual(new Solver(5, 17,
            "XXXXXXXXXXXXXXXXX" +
              "XXX.XX.XXXXXXXXXX" +
              "XX.*..M.XXXXXXXXX" +
              "XXX.XX.XXXXXXXXXX" +
              "XXXXXXXXXXXXXXXXX").waves , 1)

          assertEqual(new Solver(41, 41,
            ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X." +
              "M........................................" +
              "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX." +
              ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X." +
              "........................................." +
              ".XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" +
              ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X." +
              "........................................." +
              "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX." +
              ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X." +
              "........................................." +
              ".XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" +
              ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X." +
              "........................................." +
              "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX." +
              ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X." +
              "........................................." +
              ".XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" +
              ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X." +
              "........................................." +
              "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX." +
              ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X." +
              "........................................." +
              ".XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" +
              ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X." +
              "........................................." +
              "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX." +
              ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X." +
              "........................................." +
              ".XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" +
              ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X." +
              "........................................." +
              "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX." +
              ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X." +
              "........................................." +
              ".XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" +
              ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X." +
              "........................................." +
              "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX." +
              ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X." +
              ".*.......................................").waves , 280)

          assertNotEqual(new Solver(41, 41,
            ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X."+
            "M........................................"+
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX."+
            ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X."+
            "........................................."+
            ".XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"+
            ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X."+
            "........................................."+
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX."+
            ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X."+
            "........................................."+
            ".XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"+
            ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X."+
            "........................................."+
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX."+
            ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X."+
            "........................................."+
            ".XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"+
            ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X."+
            "........................................."+
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX."+
            ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X."+
            "........................................."+
            ".XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"+
            ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X."+
            "........................................."+
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX."+
            ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X."+
            "........................................."+
            ".XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"+
            ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X."+
            "........................................."+
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX."+
            ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X."+
            "........................................."+
            ".XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"+
            ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X."+
            "........................................."+
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX."+
            ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X."+
            ".*.......................................").waves, 281)


    assertEqual(new Solver(41, 41,
      ".X.XXXXXXXXXXXXXXXXXXX.X.X.X.X.X.X.X.X.X." +
        "...XXXXXXXXXXXXXXXXXXX..................." +
        ".X..X.X.X.X.X.X.X..XXXX*X.X.X.X.X.X.X.XX." +
        ".XXXX.X.X.X.X.X.X.XX.X.X.X.X.X.X.X.X.X.X." +
        "........................................." +
        ".XX.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X" +
        ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X." +
        "........................................." +
        "X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.XX." +
        ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X." +
        "........................................." +
        ".XX.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X" +
        ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X." +
        "........................................." +
        "X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.XX." +
        ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X." +
        "........................................." +
        ".XX.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X" +
        ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X." +
        "........................................." +
        "X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.XX." +
        ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X." +
        "........................................." +
        ".XX.X.X.X.XX.X.XX.X.X.X.X.X.X.X.X.X.X.X.X" +
        ".X.X.X.X.X.XXX.X.X.X.X.X.X.X.X.X.X.X.X.X." +
        "X........................................" +
        "X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.XX." +
        ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X." +
        "........................................." +
        ".X.XX.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.XX.XX" +
        ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.XMX." +
        ".X....................................X.." +
        "..X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.XX." +
        ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X." +
        ".X...................................X..." +
        ".XX.X.X.X.X.X.X.X.X.X.X.X.X.X.XX.XX.XXXX." +
        ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X." +
        "........................................." +
        "X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.XX." +
        ".X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X.X." +
        ".........................................").waves, 294)



  }
}
