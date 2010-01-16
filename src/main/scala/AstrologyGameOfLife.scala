package org.rosettacode.conway_life.scala

import scala.swing._
import BorderPanel.Position.{Center, South}
import Swing._
import event._
import java.awt.Dimension

object AstrologyGameOfLife extends SimpleSwingApplication {
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
      val board = new GridPanel(0, DisplayGrid.colQtd) {
        vGap = 0
        hGap = 0
        preferredSize = ((500,500): Dimension)

        // This sorting criteria inverts the rows, so that row 0
        // can be the row on the bottom instead of the top
        def coordSorting: (((Int, Int), Cell)) => (Int, Int) = {
          case ((row, col), _) => (DisplayGrid.rowQtd - row - 1, col)
        }
        for ((_, cell) <- DisplayGrid.cells.toSeq sortBy coordSorting)
          contents.append(cell)
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

