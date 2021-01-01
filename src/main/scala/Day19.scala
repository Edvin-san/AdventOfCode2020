import zio._

object Day19 extends Day[Long, Long] {
  type RuleConjunction = List[String]
  type RuleDisjunction = List[RuleConjunction]

  def parseInput(in: String): (Map[Int, RuleDisjunction], Array[String]) =
    in.split("\n\n") match {
      case Array(rulesS, messagesS) =>
        val rules = rulesS.split("\n").map(r => r.split(": ") match {
          case Array(id, ruleS) =>
            val ruleDisj = ruleS.split(" \\| ").map(_.trim.split(" ").toList).toList
            id.toInt -> ruleDisj
        }).toMap
        val messages = messagesS.split("\n")
        rules -> messages
    }

  class Translator(rules: Map[Int, RuleDisjunction]) {
    var memo: Map[Int, String] = Map()

    def ruleToRegex(id: Int): String = if (memo.isDefinedAt(id)) memo(id) else {
      val regex = rules(id).map { parts =>
        val conj = parts.map {
          case x if x.startsWith("\"") => x.replace("\"", "")
          case num =>
            val inner = ruleToRegex(num.toInt)
            if (inner.contains("|")) s"($inner)"
            else inner
        }.mkString
        conj
      }.mkString("|")
      memo = memo.updated(id, regex)
      regex
    }
  }

  def part1(in: String) = Task.effect {
    val (rules, messages) = parseInput(in)
    val translator = new Translator(rules)
    val rule0Regex = s"^${translator.ruleToRegex(0)}$$"
    val regex = rule0Regex.r
    val validMessages = messages.filter(message => regex.matches(message))
    validMessages.size
  }

  def part2(in: String) = Task.effect {
    val (rules, messages) = parseInput(in)
    if (rules.isDefinedAt(8) && rules.isDefinedAt(11)) {
      val translator = new Translator(rules)
      val r42 = translator.ruleToRegex(42)
      val r31 = translator.ruleToRegex(31)
      val r8 = s"($r42)+"
      val r11 = (1 to 4).map(i => s"(($r42){$i}($r31){$i})").mkString("|")
      val rule0Regex = s"^$r8($r11)$$"
      val regex = rule0Regex.r
      val validMessages = messages.filter(message => regex.matches(message))
      validMessages.size
    } else ???
  }

  val inputs = Map(
    "example" -> InputString(
      """0: 4 1 5
        |1: 2 3 | 3 2
        |2: 4 4 | 5 5
        |3: 4 5 | 5 4
        |4: "a"
        |5: "b"
        |
        |ababbb
        |bababa
        |abbbab
        |aaabbb
        |aaaabbb""".stripMargin),
    "example2" -> InputString(
      """42: 9 14 | 10 1
        |9: 14 27 | 1 26
        |10: 23 14 | 28 1
        |1: "a"
        |11: 42 31
        |5: 1 14 | 15 1
        |19: 14 1 | 14 14
        |12: 24 14 | 19 1
        |16: 15 1 | 14 14
        |31: 14 17 | 1 13
        |6: 14 14 | 1 14
        |2: 1 24 | 14 4
        |0: 8 11
        |13: 14 3 | 1 12
        |15: 1 | 14
        |17: 14 2 | 1 7
        |23: 25 1 | 22 14
        |28: 16 1
        |4: 1 1
        |20: 14 14 | 1 15
        |3: 5 14 | 16 1
        |27: 1 6 | 14 18
        |14: "b"
        |21: 14 1 | 1 14
        |25: 1 1 | 1 14
        |22: 14 14
        |8: 42
        |26: 14 22 | 1 20
        |18: 15 15
        |7: 14 5 | 1 21
        |24: 14 1
        |
        |abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
        |bbabbbbaabaabba
        |babbbbaabbbbbabbbbbbaabaaabaaa
        |aaabbbbbbaaaabaababaabababbabaaabbababababaaa
        |bbbbbbbaaaabbbbaaabbabaaa
        |bbbababbbbaaaaaaaabbababaaababaabab
        |ababaaaaaabaaab
        |ababaaaaabbbaba
        |baabbaaaabbaaaababbaababb
        |abbbbabbbbaaaababbbbbbaaaababb
        |aaaaabbaabaaaaababaa
        |aaaabbaaaabbaaa
        |aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
        |babaaabbbaaabaababbaabababaaab
        |aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba""".stripMargin),
    "puzzle" -> ResourceInput("day19puzzle.txt") // 311
  )
}
