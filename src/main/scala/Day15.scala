import zio._

object Day15 extends Day[Long, Long] {
  //  def p1(lastSeen: Map[Int, Int], last: Int, wasFirstTime: Boolean, turn: Int): LazyList[Int] = {
  //    val numberSpoken = if (wasFirstTime) 0 else turn - 1 - lastSeen(last)
  //    println(s"last: $last, wasFirstTime: $wasFirstTime, turn: $turn, the number spoken this turn is $numberSpoken, lastSeen: $lastSeen")
  //    numberSpoken #:: p1(lastSeen.updated(last, turn - 1), numberSpoken, !lastSeen.isDefinedAt(numberSpoken), turn + 1)
  //  }
  def p1(firstSeen: Map[Int, Int], prevSeen: Map[Int, Int], lastSeen: Map[Int, Int], last: Int, turn: Int): LazyList[Int] = {
    //    if (turn % 100000 == 0) println(turn)
    val numberSpoken = if (firstSeen(last) == lastSeen(last)) 0 else lastSeen(last) - prevSeen(last)
    //    println(s"turn: $turn, last: $last, the number spoken this turn is $numberSpoken, lastSeen: $lastSeen, $prevSeen, $firstSeen")
    val newFirstSeen = if (lastSeen.isDefinedAt(numberSpoken)) firstSeen else firstSeen.updated(numberSpoken, turn)
    val newPrevSeen = if (lastSeen.isDefinedAt(numberSpoken)) prevSeen.updated(numberSpoken, lastSeen(numberSpoken)) else prevSeen
    val newLastSeen = lastSeen.updated(numberSpoken, turn)
    numberSpoken #:: p1(newFirstSeen, newPrevSeen, newLastSeen, numberSpoken, turn + 1)
  }

  def part1(in: String) = Task.effect {
    val preamble = in.split(",").map(_.toInt)
    val lastSeen = preamble.zipWithIndex.toMap.view.mapValues(_ + 1).toMap
    val sequence = LazyList.from(preamble) #::: p1(lastSeen, lastSeen, lastSeen, preamble.last, preamble.size + 1)
    sequence.drop(2019).head
  }

  def part2(in: String) = Task.effect {
    val preamble = in.split(",").map(_.toInt)
    val lastSeen = preamble.zipWithIndex.toMap.view.mapValues(_ + 1).toMap
    val sequence = LazyList.from(preamble) #::: p1(lastSeen, lastSeen, lastSeen, preamble.last, preamble.size + 1)
    sequence.drop(30000000 - 1).head
  }

  val inputs = Map(
    "example" -> InputString("0,3,6"),
    "example1" -> InputString("1,3,2"),
    "example2" -> InputString("2,1,3"),
    "example3" -> InputString("1,2,3"),
    "example4" -> InputString("2,3,1"),
    "example5" -> InputString("3,2,1"),
    "example6" -> InputString("3,1,2"),
    "puzzle" -> InputString("19,20,14,0,9,1")
  )
}
