import zio._

object Day21 extends Day[Long, String] {
  case class Allergen(value: String) extends AnyVal
  case class Ingredient(value: String) extends AnyVal

  def parseInput(in: String): List[(List[Ingredient], List[Allergen])] =
    in.split("\n").map { line =>
      val Array(ing, alg) = line.split("\\(contains ")
      val ingr = ing.trim.split(" ").map(Ingredient(_)).toList
      val allergens = alg.dropRight(1).split(", ").map(Allergen(_)).toList
      (ingr, allergens)
    }.toList

  def allergenConstraints(foods: List[(List[Ingredient], List[Allergen])], acc: Map[Allergen, Set[Ingredient]]): Map[Allergen, Set[Ingredient]] = foods match {
    case (ingredients, allergens) :: tail =>
      val newConstraints = allergens.foldLeft(acc)((constr, allergen) => constr.updated(allergen, constr(allergen) intersect ingredients.toSet))
      allergenConstraints(tail, newConstraints)
    case Nil => acc
  }

  def refineConstraints(constraints: Map[Allergen, Set[Ingredient]]): Map[Allergen, Set[Ingredient]] = {
    val determined = constraints.filter(_._2.size == 1).flatMap(_._2).toSet
    val newConstraints = constraints.map {
      case (allergen, ingredients) => allergen -> (if (ingredients.size > 1) ingredients -- determined else ingredients)
    }
    if (newConstraints == constraints) constraints
    else refineConstraints(newConstraints)
  }

  def part1(in: String) = Task.effect{
    val foods = parseInput(in)
    val allIngredients = foods.flatMap(_._1).toSet
    val initialConstraints = foods.flatMap(_._2).map(a => a -> allIngredients).toMap
    val deducedConstraints = allergenConstraints(foods, initialConstraints)
    val refinedConstraints = refineConstraints(deducedConstraints)
    val constrainedIngredients = refinedConstraints.flatMap(_._2).toSet
    val unseenIngredients = allIngredients.filter(i => !constrainedIngredients.contains(i))
    val numUnseen = foods.flatMap(_._1).count(unseenIngredients(_))
    println(refinedConstraints.mkString("\n"))
    println(unseenIngredients)
    numUnseen
  }

  def part2(in: String) = Task.effect{
    val foods = parseInput(in)
    val allIngredients = foods.flatMap(_._1).toSet
    val initialConstraints = foods.flatMap(_._2).map(a => a -> allIngredients).toMap
    val deducedConstraints = allergenConstraints(foods, initialConstraints)
    val refinedConstraints = refineConstraints(deducedConstraints)
    refinedConstraints.filter(_._2.size == 1).view.mapValues(_.head).toList.sortBy(_._1.value).map(_._2.value).mkString(",")
  }

  val inputs = Map(
    "example" -> InputString("""mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
                               |trh fvjkl sbzzf mxmxvkd (contains dairy)
                               |sqjhc fvjkl (contains soy)
                               |sqjhc mxmxvkd sbzzf (contains fish)""".stripMargin),
    "puzzle" -> ResourceInput("day21puzzle.txt")
  )
}
