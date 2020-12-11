import zio._

object Day10 extends Day[Long, Long] {
  def parseAdapters(s: String) = {
    val otherAdapters = s.split("\n").map(_.toInt).sorted
    otherAdapters.prepended(0).appended(otherAdapters.max + 3)
  }

  def part1(in: String) = Task.effect {
    val adapters = parseAdapters(in)
    val diffs = adapters.tail.zip(adapters.dropRight(1)).map { case (x, y) => x - y }
    val diffCounts = diffs.groupBy(identity).view.mapValues(_.size).toMap
    diffCounts.getOrElse(1, 0) * diffCounts.getOrElse(3, 0)
  }

  def part2(in: String) = Task.effect {
    val adapters = parseAdapters(in).toList
    var memo = Map[Int, Long]()
    def ways(i: Int): Long = memo.get(i) match {
      case Some(value) => value
      case None => if (i == adapters.size - 1) 1 else {
        val jolts = adapters(i)
        val candidateOffsets = adapters.drop(i + 1).takeWhile(_ - jolts <= 3).zipWithIndex.map(_._2 + 1)
        assert(candidateOffsets.size > 0)
        val myWays = candidateOffsets.map(offset => ways(i + offset)).sum
        memo = memo + (i -> myWays)
        myWays
      }
    }

    ways(0)
  }

  val inputs = Map(
    "exampleSmall" -> InputString(
      """16
        |10
        |15
        |5
        |1
        |11
        |7
        |19
        |6
        |12
        |4""".stripMargin),
    "exampleLarger" -> InputString(
      """28
        |33
        |18
        |42
        |31
        |14
        |46
        |20
        |48
        |47
        |24
        |23
        |49
        |45
        |19
        |38
        |39
        |11
        |1
        |32
        |25
        |35
        |8
        |17
        |7
        |9
        |4
        |2
        |34
        |10
        |3""".stripMargin),
    "puzzle" -> InputString(
      """99
        |3
        |1
        |11
        |48
        |113
        |131
        |43
        |82
        |19
        |4
        |153
        |105
        |52
        |56
        |109
        |27
        |119
        |147
        |31
        |34
        |13
        |129
        |17
        |61
        |10
        |29
        |24
        |12
        |104
        |152
        |103
        |80
        |116
        |79
        |73
        |21
        |133
        |44
        |18
        |74
        |112
        |136
        |30
        |146
        |100
        |39
        |130
        |91
        |124
        |70
        |115
        |81
        |28
        |151
        |2
        |122
        |87
        |143
        |62
        |7
        |126
        |95
        |75
        |20
        |123
        |63
        |125
        |53
        |45
        |141
        |14
        |67
        |69
        |60
        |114
        |57
        |142
        |150
        |42
        |78
        |132
        |66
        |88
        |140
        |139
        |106
        |38
        |85
        |37
        |51
        |94
        |98
        |86
        |68
        |""".stripMargin)
  )
}
