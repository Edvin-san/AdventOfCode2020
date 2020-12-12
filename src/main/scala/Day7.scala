import zio._

import scala.io.Source
import scala.util.Try

object Day7 extends Day[Long, BigInt] {

  case class Bag(value: String) extends AnyVal

  case class BagSpec(bag: Bag, content: Map[Bag, Long])

  def parseLine(line: String): Try[BagSpec] = Try {
    val l = line.replace("bags", "bag").dropRight(1).split(" contain ")
    val bag = l(0)
    val bagCounts = l(1).split(", ")
    val m: Map[Bag, Long] = bagCounts.flatMap {
      case "no other bag" => None
      case s => s.split(" ").toList match {
        case num :: b => Some(Bag(b.mkString(" ")) -> num.toLong)
        case _ => ???
      }
    }.toMap
    BagSpec(Bag(bag), m)
  }

  def part1(in: String) =
    Task.effect {
      val bagSpecs = Source.fromString(in).getLines().map(parseLine).toList.map(_.get)
      val m = bagSpecs.map(b => (b.bag, b)).toMap
      val target = Bag("shiny gold bag")
      var memo: Map[Bag, Boolean] = Map(target -> true)

      def containsTarget(bag: Bag): Boolean = memo.get(bag) match {
        case Some(value) => value
        case None =>
          val contains = m(bag).content.map { case (innerBag, _) => containsTarget(innerBag) }.fold(false)(_ || _)
          memo = memo + (bag -> contains)
          contains
      }

      val res = bagSpecs.map(bs => (bs.bag, containsTarget(bs.bag)))
      res.map(_._2).filter(_ == true).size - 1
    }

  def part2(in: String) = Task.effect {
    val bagSpecs = Source.fromString(in).getLines().map(parseLine).toList.map(_.get)
    val m = bagSpecs.map(b => (b.bag, b)).toMap
    var memo: Map[Bag, BigInt] = Map()

    def nestedBags(bag: Bag): BigInt = memo.get(bag) match {
      case Some(value) => value
      case None =>
        val numBags = m(bag).content.map { case (innerBag, amount) => (nestedBags(innerBag) + 1) * amount }.fold(BigInt(0))(_ + _)
        memo = memo + (bag -> numBags)
        numBags
    }

    nestedBags(Bag("shiny gold bag"))
  }

  val inputs = Map(
    "example" -> InputString(
      """light red bags contain 1 bright white bag, 2 muted yellow bags.
        |dark orange bags contain 3 bright white bags, 4 muted yellow bags.
        |bright white bags contain 1 shiny gold bag.
        |muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
        |shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
        |dark olive bags contain 3 faded blue bags, 4 dotted black bags.
        |vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
        |faded blue bags contain no other bags.
        |dotted black bags contain no other bags.""".stripMargin),
    "example2" -> InputString(
      """shiny gold bags contain 2 dark red bags.
        |dark red bags contain 2 dark orange bags.
        |dark orange bags contain 2 dark yellow bags.
        |dark yellow bags contain 2 dark green bags.
        |dark green bags contain 2 dark blue bags.
        |dark blue bags contain 2 dark violet bags.
        |dark violet bags contain no other bags.""".stripMargin),
    "puzzle" -> ResourceInput("day7puzzle.txt")
  )
}
