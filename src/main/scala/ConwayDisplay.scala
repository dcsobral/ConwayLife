// By Pedro Furlanetto 

import scala.swing._
import Swing._

object gui extends SimpleSwingApplication {
  val colQtd = 50
  val rowQtd = 50
  val cellSize = 10
  val colors = Array(java.awt.Color.white, java.awt.Color.blue, java.awt.Color.yellow, java.awt.Color.green) 
  
  var cells = Array.ofDim[Cell](colQtd, rowQtd)  
  
  class Cell(text:String) extends Button(text) {
    var currColor = 0
    preferredSize = ((cellSize,cellSize): Dimension)
    background = colors(0)  
    border = Swing.EmptyBorder //Swing.LineBorder(java.awt.Color.black,1)
    def switch = background = {   
      val inc = if(currColor==(colors.length-1)) (1-colors.length) else 1
      currColor += inc
      colors(currColor)
    } 
  }
  
  def top = new MainFrame {
    title = "Conway Game of Life"
    contents = new GridPanel(0,colQtd) {
      vGap = 0
      hGap = 0
      //preferredSize_=(200,200)
      (1 to colQtd).foreach { x => 
        (1 to rowQtd).foreach { y =>
          val cell = new Cell(" ")
          contents.append(cell)
          cells(x - 1)(y - 1)=cell
        }
      }
    }
  }
  
  override def main(args: Array[String]): Unit = {
    // super.main(args); 
    Swing.onEDTWait { startup(args) }
    val rand = new java.util.Random();
    while (true) {
      cells(rand.nextInt(colQtd-1))(rand.nextInt(rowQtd-1)).switch
      Thread.sleep(5)
    }  
  }
}

