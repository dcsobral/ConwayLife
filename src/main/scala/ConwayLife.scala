package org.rosettacode.conway_life.scala

import scala.swing._
import BorderPanel.Position.{Center, South}
import java.awt.Color

/**
 * Created by IntelliJ IDEA.
 * User: BT024593
 * Date: 12/01/2010
 * Time: 15:53:13
 * To change this template use File | Settings | File Templates.
 */

object ConwayLife extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Conway's Life for Scala"

    menuBar = new MenuBar {
      contents += new Menu("File") {
        contents += new MenuItem("Save")
        contents += new MenuItem("Save as")
        contents += new MenuItem("Load")
        contents += new Separator
        contents += new MenuItem("Exit")
      }
      contents += new Menu("Edit") {
        contents += new MenuItem("Clear")
      }
    }

    contents = new BorderPanel {
      val buttons = new GridPanel(1, 1) {
        contents += new Label("Actions:")
        contents += new Button("Play")
        contents += new Button("Pause")
        contents += new Button("Clear")
        contents += new Separator
        contents += new Label("Patterns:")
        contents += new Label("Still Lives")
        contents += new Button("Block")
        contents += new Button("Beehive")
        contents += new Button("Loaf")
        contents += new Button("Boat")
        contents += new Label("Oscillators")
        contents += new Button("Blinker")
        contents += new Button("Toad")
        contents += new Button("Beacon")
        contents += new Button("Pulsar")
        contents += new Button("Eight")
        rows = contents.size
      }
      val board = new Panel {
        background = Color.white
        listenTo(mouse.clicks, mouse.moves)
      }

      val center = new SplitPane(Orientation.Vertical, buttons, board)
      layout(center) = Center

      object slider extends Slider {
        min = 0
        value = 0
        max = 10
        majorTickSpacing = 1
      }
      layout(slider) = South
    }
  }
}
