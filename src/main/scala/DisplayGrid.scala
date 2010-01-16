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
  
  val cells = Map(Seq.tabulate(rowQtd, colQtd)(
    (_, _) -> new Cell((_: Cell).switch)
  ).flatten: _*)
}
