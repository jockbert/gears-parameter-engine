package se.mjukomp.gears.examples

import se.mjukomp.gears.Parameter

trait ExamplePart {}

case class DescriptionPart(textParagraphs: String*) extends ExamplePart

case class ParameterPart(name: String, parameter: Parameter) extends ExamplePart
