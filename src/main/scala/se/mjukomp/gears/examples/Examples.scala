package se.mjukomp.gears.examples

import se.mjukomp.gears._
import se.mjukomp.gears.Parameter._

object Examples {

  def examples: Seq[Example] = Nil

  def inverseFunctionOf = {
    val liters = Parameter("l/100km", 10, 0, 5)
    val mpg = Parameter("mpg", 50, 0, 200)

    Relation.relate(liters).asInverseFunctionOf(mpg, mpg => 282.48 / mpg)

    Example(
      "inv. func",
      DescriptionPart("Two parameters with a inverse function relation in between them"),
      ParameterPart("Liter per 100 km", liters),
      ParameterPart("UK miles per galon", mpg))
  }
}
