package org.rosettacode.conway_life.scala

/**
 * Created by IntelliJ IDEA.
 * User: BT024593
 * Date: 13/01/2010
 * Time: 17:00:19
 * To change this template use File | Settings | File Templates.
 */

object DisplayGrid {
  val colQtd=50
  val rowQtd=50
  val center = Coord(colQtd / 2, rowQtd / 2)
  private var internalGrid = Generation.empty
  def displayable(coord: Coord) = (0 until colQtd contains coord.x) && (0 until rowQtd contains coord.y)

  def clear {
    internalGrid.alive foreach (this(_) = false)
  }

  def grid = internalGrid
  def grid_=(g: Generation) = {
    // println("Present:"+internalGrid)
    // println("Next: "+g.alive)
    // println("To clear: "+internalGrid.filterNot(g.alive contains))
    internalGrid filterNot (g.alive contains) foreach (this(_) = false)
    g.alive foreach (this(_) = true)
  }
  def apply(coord: Coord) = internalGrid contains coord
  def update(coord: Coord, setAlive: Boolean): Unit =
    if (setAlive) {
      internalGrid += coord
      if (displayable(coord)) cells(coord).alive
    } else {
      internalGrid -= coord
      if (displayable(coord)) cells(coord).dead
    }
  def switch(coord: Coord) = this(coord) = ! this(coord)
  
  val cells = Map(Seq.tabulate(colQtd, rowQtd)( (x, y) =>
    Coord(x, y) -> new Cell(cell => this(Coord(x, y)) = ! cell.isAlive)
  ).flatten: _*)
}
