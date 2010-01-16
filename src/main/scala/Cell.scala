package org.rosettacode.conway_life.scala

import scala.swing._
import Swing._
import event._
import collection.immutable.Queue
import java.awt.{Color, Dimension}

/**
 * Created by IntelliJ IDEA.
 * User: BT024593
 * Date: 13/01/2010
 * Time: 11:38:57
 * To change this template use File | Settings | File Templates.
 */

class Cell(callback: (Cell) => Unit) extends Button(" ") {
  val cellSize=10
  var colorQueue = Queue[Color]()
  def aliveColor = Color.green
  def deadColor = Color.white
  def alive = {
    background = aliveColor
    colorQueue = Queue(deadColor, aliveColor)
  }
  def dead = {
    background = deadColor
    colorQueue = Queue(aliveColor, deadColor)
  }
  def isAlive = background == aliveColor

  def nextColor = {
    val (color, queue) = colorQueue.dequeue
    colorQueue = queue enqueue color
    color
  }
  def switch = background = nextColor

  size = ((cellSize, cellSize) : Dimension)
  border = Swing.EmptyBorder //Swing.LineBorder(java.awt.Color.black,1)
  focusPainted = false
  dead

  reactions += {
    case ButtonClicked(_) => callback(this)
  }
}
