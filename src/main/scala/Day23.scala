import zio._

object Day23 extends Day[String, Long] {
  def sim1move(nums: List[Int]): List[Int] = nums match {
    case head :: tail =>
      val (threeCups, rest) = tail.splitAt(3)
      val destCup = rest.filter(_ < head).maxOption.getOrElse(rest.max)
      val (firstpart, endpart) = rest.splitAt(rest.indexOf(destCup) + 1)
      firstpart ::: threeCups ::: endpart.appended(head)
    case _ => ???
  }

  class Cup(val value: Int, var destCup: Option[Cup], var next: Option[Cup]) {
    def getDest = destCup.get
    def getNext = next.get
    def setNext(c: Cup) = next = Some(c)
    def prettyPrint: String = s"($value, d:${destCup.map(_.value.toString).getOrElse("NULL")}, n:${next.map(_.value.toString).getOrElse("NULL")}}"
  }
  // Linked list can make a move O(1)
  class FastSimulator(initialNums: List[Int]) {
    val (first, one) = makeInitialCups
    private def makeInitialCups: (Cup, Cup) = {
      val firstTen = Array.ofDim[Option[Cup]](11)
      val first10Nums = initialNums.take(10) // always ends with 10
      val first = first10Nums.head
      firstTen(first) = Some(new Cup(first, None, None))
      for (pairs <- first10Nums.sliding(2)) {
        pairs match {
          case List(i, j) =>
            firstTen(j) = Some(new Cup(j, None, None))
            firstTen(i).get.next = Some(firstTen(j).get)
        }
      }
      for (i <- first10Nums.filter(_ != 1)) {
        firstTen(i).get.destCup = firstTen(i - 1)
      }
      var last = firstTen(first10Nums.last).get
      for (i <- initialNums.drop(10)) {
        val c = new Cup(i, Some(last), None)
        last.setNext(c)
        last = c
      }
      last.next = firstTen(first)
      firstTen(1).get.destCup = Some(if (last.value < 10) firstTen(first10Nums.max).get else last)

      (firstTen(first).get, firstTen(1).get)
    }
    var cur = first
    def move = {
      val (f1, f2, f3) = (cur.getNext, cur.getNext.getNext, cur.getNext.getNext.getNext)
      cur.setNext(f3.getNext)
      val pickedUp = List(f1.value, f2.value, f3.value)
      val dest = LazyList.iterate(cur.getDest)(_.getDest).dropWhile(c => pickedUp.contains(c.value)).head
      f3.setNext(dest.getNext)
      dest.setNext(f1)
      cur = cur.getNext
    }
    def getListFrom(c: Cup): List[Int] = c.value :: LazyList.iterate(c.getNext)(_.getNext).takeWhile(_.value != c.value).map(_.value).toList
  }

  def part1Naive(in: String) = Task.effect{
    val nums = in.map(_.asDigit).toList
    println(nums)
    val sequence = LazyList.iterate(nums)(sim1move)
    val finalnums = sequence(100)
    val (beforeOne, oneAndAfter) = finalnums.splitAt(finalnums.indexOf(1))
    val withoutOne = oneAndAfter.drop(1) ::: beforeOne
    withoutOne.mkString
  }

  def part1(in: String) = Task.effect{
    val nums = in.map(_.asDigit).toList

    val solver = new FastSimulator(nums)
    var i = 0
    while (i < 100) {
      solver.move
      i = i + 1
    }
    val withoutOne = solver.getListFrom(solver.one).drop(1)
    withoutOne.mkString
  }

  def part1Zio(in: String) =
    for {
      nums <- UIO(in.map(_.asDigit).toList)
      solver = new FastSimulator(nums)
      _ <- UIO(solver.move).repeatN(99)
      withoutOne = solver.getListFrom(solver.one).drop(1)
    } yield withoutOne.mkString

  // Looks nice but takes 2x more time.
  def part2Zio(in: String) =
    for {
      numsPreamble <- UIO(in.map(_.asDigit).toList)
      solver = new FastSimulator(numsPreamble ::: ((numsPreamble.max + 1) to 1000000).toList)
      _ <- UIO(solver.move).repeatN(10000000 - 1)
    } yield List(solver.one.getNext.value, solver.one.getNext.getNext.value).map(_.toLong).product

  def part2(in: String) = Task.effect{
    val numsPreamble = in.map(_.asDigit).toList
    val nums = numsPreamble ::: ((numsPreamble.max + 1) to 1000000).toList

    val solver = new FastSimulator(nums)
    var i = 0
    while (i < 10000000) {
      solver.move
      i = i + 1
    }
    List(solver.one.getNext.value, solver.one.getNext.getNext.value).map(_.toLong).product
  }

  val inputs = Map(
    "example" -> InputString("389125467"),
    "puzzle" -> InputString("871369452")
  )
}
