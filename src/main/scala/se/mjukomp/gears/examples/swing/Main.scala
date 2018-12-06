package se.mjukomp.gears.examples.swing

import scala.swing._

object Main extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Gears example"
    size = new Dimension(300,400)
    
    contents = new Button {
      text = "Click Me!"
    }
  }
}