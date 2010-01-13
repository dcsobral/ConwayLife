package org.rosettacode.conway_life.scala

import scala.swing._
import Swing._
import event._
import collection.immutable.Queue
import java.awt.Dimension

/**
 * Created by IntelliJ IDEA.
 * User: BT024593
 * Date: 13/01/2010
 * Time: 11:38:57
 * To change this template use File | Settings | File Templates.
 */

class Cell(callback: (Cell) => Unit) extends Button(" ") {
  val cellSize=10
  var colorQueue = Queue(java.awt.Color.white, java.awt.Color.green)

  def nextColor = {
    val (color, queue) = colorQueue.dequeue
    colorQueue = queue enqueue color
    color
  }

  size = ((cellSize, cellSize) : Dimension)
  background = nextColor
  border = Swing.EmptyBorder //Swing.LineBorder(java.awt.Color.black,1)
  focusPainted = false

  reactions += {
    case ButtonClicked(_) => callback(this)
  }

  def switch = background = nextColor
}
