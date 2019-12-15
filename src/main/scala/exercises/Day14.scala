package exercises

import util.DayN

object Day14 extends DayN {
  override val num = 14

  case class Chemical(amount: Long, name: String)

  case class Recipe(ingredients: List[Chemical], result: Chemical) {
    def *(multiple: Long) = Recipe(
      ingredients.map(i => i.copy(amount = i.amount * multiple)),
      result.copy(amount = result.amount * multiple)
    )
  }

  case class State(ingredients: Map[String, Long], ore: Long)

  def minimumFuel(recipes: Map[String, Recipe]): Long = {
    @annotation.tailrec
    def sort(empties: Set[String], connections: Map[String, Set[String]], result: List[String]): List[String] = {
      if (empties.isEmpty) result
      else {
        val newConnections = connections.mapValues(_.filterNot(_ == empties.head)) - empties.head
        val newEmpties = empties ++ newConnections.filter(_._2.isEmpty).keySet - empties.head
        sort(newEmpties, newConnections, empties.head :: result)
      }
    }

    @annotation.tailrec
    def loop(nodes: List[String], ingredients: Map[String, Long]): Long = nodes match {
      case Nil => 0
      case "ORE" :: _ => ingredients("ORE")
      case h :: t =>
        val recipe = recipes(h)
        val amount = ingredients(h)
        val multiple = (amount + recipe.result.amount - 1) / recipe.result.amount
        val newIngredients = (recipe * multiple).ingredients
        val updatedIngredients = newIngredients.foldLeft(ingredients) { case (b, c) => b + (c.name -> (b.getOrElse(c.name, 0L) + c.amount)) }

        loop(t, updatedIngredients)
    }

    val sorted = sort(Set("ORE"), recipes.mapValues(_.ingredients.map(_.name).toSet), Nil)
    loop(sorted, Map("FUEL" -> 1))
  }

  def moreExpensiveFuel(multiple: Int, recipes: Map[String, Recipe]): Long = {
    val fuelRecipe = recipes("FUEL")
    val newFuelRecipe = fuelRecipe * multiple
    val modifiedRecipes = recipes.updated("FUEL", newFuelRecipe)
    minimumFuel(modifiedRecipes)
  }

  private def parseInput(lines: List[String]): List[Recipe] = {
    def parseChemical(s: String): Chemical = {
      val split = s.split(" ")
      Chemical(split(0).toInt, split(1))
    }

    lines.map(_.split(" => ")).map(a => {
      val result = parseChemical(a(1))
      val ingredients = a(0).split(", ").map(parseChemical).toList
      Recipe(ingredients, result)
    })
  }

  val basic = "9 ORE => 2 A\n8 ORE => 3 B\n7 ORE => 5 C\n3 A, 4 B => 1 AB\n5 B, 7 C => 1 BC\n4 C, 1 A => 1 CA\n2 AB, 3 BC, 4 CA => 1 FUEL".split("\n").toList
  val middle = "171 ORE => 8 CNZTR\n7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL\n114 ORE => 4 BHXH\n14 VRPVC => 6 BMBT\n6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL\n6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT\n15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW\n13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW\n5 BMBT => 4 WPTQ\n189 ORE => 9 KTJDG\n1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP\n12 VRPVC, 27 CNZTR => 2 XDBXC\n15 KTJDG, 12 BHXH => 5 XCVML\n3 BHXH, 2 VRPVC => 7 MZWV\n121 ORE => 7 VRPVC\n7 XCVML => 6 RJRHP\n5 BHXH, 4 VRPVC => 5 LTCX".split("\n").toList
  val input = parseInput(lines).map(r => r.result.name -> r).toMap

  val minimum = minimumFuel(input)
  part1(minimum)

  // WELL THIS WAS SLOWER THAN I'D HOPED
  //  val max = 1000000000000L
  //  part2(Stream.from(1).map(i => i -> moreExpensiveFuel(i, input)).collectFirst { case (i, l) if (max - l) < 0 => i-1 })

  // HACKY BINARY SEARCH CAUSE LAZY
  val i = 1122036
  part2(1000000000000L - moreExpensiveFuel(i, input))
}
