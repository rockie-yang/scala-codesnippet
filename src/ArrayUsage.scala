
object ArrayUsage {

  def toString[T](ar: Array[T]) = ar.mkString(",")

  val a = Array(1, 2)
  val b = Array(4, 5)
  val c = a ++ b

  def egConcat(): Unit = {
    val arrayEqualsWillSurpriseU = c.equals(Array(1, 2, 4, 5))
    assert(toString(c) == "1,2,4,5")

    // prepend an element in the front
    // notice: + is in the front, and : is in the back
    // +: is right associative, which means +: is an function of Array
    assert(toString(3 +: a) == "3,1,2")

    // append an element
    assert(toString(a :+ 3) == "1,2,3")
  }

  def id(obj: AnyRef) =
    Integer.toHexString(System.identityHashCode(obj))

  def egToString() {
    val sb1 = new StringBuilder   // sb1 is "" now
    val id1 = id(sb1)

    val sb2 = c.addString(sb1)  // content in c added to sb1 without separator
    val id2 = id(sb2)

    // sb1 sb2 are the same object
    // not only the content is the same
    assert(id1 == id2)
    assert(sb1.toString() == "1245") // sb1.toString === sb2.toString == "1245

    // Array.addString(sb: StringBuilder, sep: String)
    //   ===> is the same with
    // Array.mkString(sep: String)
    //   if
    // the sb is empty at the time
    val sb3 = c.addString(sb1, ",")
    assert(sb3.toString() == "12451,2,4,5")
    assert(sb3.toString() == "1245" + c.mkString(","))


    // Array.addString(sb: StringBuilder, start: String, sep: String, end: String)
    // ===> is the same with
    // Array.mkString(start: String, sep: String, end: String)
    val sb4 = c.addString(new StringBuilder(), "start: (", ",", ")")
    assert(sb4.toString() == "start: (1,2,4,5)")
    assert(sb4.toString() == c.mkString("start: (", ",", ")"))

    // mkString should be used in most case, one time convert to string
    // addString should be used if want to continually add up

    // using deep.toString to get more readable output
    // this can be only used to array and multiple level of array
    assert(c.deep.toString() == "Array(1, 2, 4, 5)")
  }

  def egCollect(): Unit = {
    // collect can be used the same as filter if the value is not mapped
    // a bit verbose then filter through
    val asFilter = c.collect{case i if i % 2 == 0 => i}
    val filter = c.filter(_ % 2 == 0)
    assert(toString(asFilter) == toString(filter))

    // collect should be mostly used as a combination of filter and map
    val even1 = c.collect{case i if i % 2 == 0 => f"$i is even"}
    val even2 = c.filter(_ % 2 == 0).map{i => f"$i is even"}
    assert(toString(even1) == "2 is even,4 is even")
    assert(toString(even1) == toString(even2))

    // collectFirst can be used the same as find if the value is not mapped
    val asFind = c.collectFirst {case i if i % 2 == 0 => i}
    val find = c.find(_ % 2 == 0)
    assert(asFind.get == find.get)

    // collectFirst should be mostly used as a combination of find and map
    val firstEven = c.collectFirst {case i if i % 2 == 0 => f"$i is the first even"}
    assert(firstEven.get == "2 is the first even")

    // None will be returned if the value does not exist
    val notFound =  c.collectFirst {case i if i > 10 => i}
    assert(notFound.isEmpty)
  }

  def egCombination(): Unit = {
    // get all identical combinations with one element
    // get 1 element from (1,2) should got (1) (2)
    val comb1 = a.combinations(1).toSet
    // Set with Array elements can not be simply compare the equality
    // because array can not be simply compare the equality :-(
    assert(comb1 != Set(Array(1), Array(2)))

    // while other collection rather than array can compare the equality
    assert(a.toList.combinations(1).toSet == Set(List(1), List(2)))

    // get 3 elements from (1,2,4,5) should got
    // (1,2,4) (1,2,5) (1,4,5) (2,4,5)
    val comb3 = c.toList.combinations(3).toSet
    assert(comb3 == Set(List(1,2,4), List(1,2,5), List(1,4,5), List(2,4,5)))
  }

  def egCopy(): Unit = {
    // copy from a bigger array to a small array
    // will NOT get exception
    // the smaller part will be copied
    val arSmaller = new Array[Int](3)
    c.copyToArray(arSmaller)
    assert(arSmaller.deep.toString() == "Array(1, 2, 4)")

    // copy from a smaller array to a bigger array
    // will keep extra elements unchanged
    val arBigger = new Array[Int](5)
    assert(arBigger.deep.toString() == "Array(0, 0, 0, 0, 0)")
    c.copyToArray(arBigger)
    assert(arBigger.deep.toString() == "Array(1, 2, 4, 5, 0)")

    // Array.copyToArray(xs: Array, start: Int)
    // copy source array to the dest array at start point
    // the start is the place at dest array
    val arStart = new Array[Int](3)
    c.copyToArray(arStart, 2)
    assert(arStart.deep.toString() == "Array(0, 0, 1)")

    // limit the number of element to be copied
    // exceed the length of dest array will be ignored
    c.copyToArray(arStart, 1, 3)
    assert(arStart.deep.toString() == "Array(0, 1, 2)")

    // distinct build a new array
    // it is decoupled between the source array and dest array
    // change one array will not affect the other
    val d = c.distinct
    d(1) = 3
    assert(c.deep.toString() == "Array(1, 2, 4, 5)")
    // TODO copyToBuffer
  }

  def egPredicat(): Unit = {
    // test if an element is existed in the array
    assert(c.contains(1))

    // test a slice is existed in the array
    // the slice does not have to be the same collection
    // it only need be SeqLike
    assert(c.containsSlice(List(2, 4)))

    // check if the array ends with a Seq
    assert(c.endsWith(List(4,5)))
    assert(!c.endsWith(Array(4,6)))

    // check if exist any element that satisfy the predict
    // it is similar with contains, just contain only can test _ == el
    assert(c.exists(_ % 2 == 0))

    // check if all elements satisfy the predict
    assert(c.forall(_ > 0))
  }

  def egCount(): Unit = {
    // get how many elements satisfy a predict
    assert(c.count(_ % 2 == 0) == 2)

    // through we can use filter and length to get the count as well
    // while that takes more space
    assert(c.filter(_ % 2 == 0).length == 2)

  }

  def egPick(): Unit ={
    // get all elements except the first n
    assert(c.drop(2).deep.toString() == "Array(4, 5)")

    // get all elements except the last n
    assert(c.dropRight(2).deep.toString() == "Array(1, 2)")

    // dropWhile drop all the element DOES satisfy the predict
    // until comes to an element does NOT  satisfy the predict
    assert(c.dropWhile(_ < 3).deep.toString() == "Array(4, 5)")

    // filter    get the element does     satisfy the predict
    // filterNot get the element does NOT satisfy the predict
    assert(c.filter(_ % 2 == 0).deep.toString() == "Array(2, 4)")
    assert(c.filterNot(_ % 2 == 0).deep.toString() == "Array(1, 5)")

    // find get the FIRST element if exist
    // or None if not exist
    assert(c.find(_ == 3).isEmpty)
    assert(c.find(_ % 2 == 0).get == 2)

  }

  def egMap(): Unit ={
    val lines = Array("The world is a book.", "If you do not travel,", "you only read a page")

    // the map below will generate array of array
    // and then we flatten it
    val map = lines.map(line => line.split("\\W+"))
    val flatten = map.flatten

    // flatMap is the combination of map and flatten
    val words = lines.flatMap(line => line.split("\\W+"))
    assert(words.deep.toString() == flatten.deep.toString)
  }

  // TODO
  // foreach
  // groupBy grouped
  // head, tail
  // last, init
  // indexOf

  def egFold(): Unit ={
    val ar = Array(2, 4, 5)

    // 1 is the initial value
    // it first apply on the first  element 2, (i, result) is (2, 1) => 1 * 2 = 2
    // it then  apply on the second element 4, (i, result) is (4, 2) => 4 * 2 = 8
    // it then  apply on the third  element 5, (i, result) is (5, 8) => 5 * 8 = 40
    // so the result a single value 40
    //
    // item is in the left and result is in the right
    // because this is foldRight, fold to the right
    // that is also /: the slash is lean to the right
    //
    // that is also why result is the first element and i is the second
    // because we first have the initial result, then lean to the right
    val toTheRight = (1 /: ar){
      (result, i) =>
//        println(result, i)
        i * result
    }
    val foldRight = ar.foldRight(1){(i, result) => i * result}
    // in this case, we did a product so it's the same with ar.product()
    assert(toTheRight == foldRight)

    // 1 is the initial value
    // it first apply on the last   element 5, (i, result) is (2, 1) => 1 + 5 * 5 = 26
    // it then  apply on the second element 4, (i, result) is (4, 2) => 4 * 2 = 8
    // it then  apply on the third  element 5, (i, result) is (5, 8) => 5 * 8 = 40
    // so the result a single value 40
    //
    // item is in the left and result is in the right
    // because this is foldRight, fold to the right
    // that is also /: the slash is lean to the right
    //
    // that is also why result is the first element and i is the second
    // because we first have the initial result, then lean to the right
    val toTheLeft = (ar :\ 1){
      (i, result) =>
        println(i, result)
        i * i + result
    }
    val foldLeft = ar.foldLeft(1){(result, i) => i * i + result}

    assert(toTheLeft == foldLeft)
  }

  def main(args: Array[String]) {
    egFold()
    egMap()
    egPick()
    egConcat()
    egToString()
    egCollect()
    egCombination()
    egCopy()
    egPredicat()
    egCount()
  }

  // TODO aggregate is more general form of fold and reduce

}
