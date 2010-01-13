package swing

import scala.swing._
import scala.swing.event._
import scala.swing.Swing._

object AstrologyGameOfLife extends SimpleSwingApplication {
  val colQtd=50
  val rowQtd=50
  val cellSize=10
  val colors=Array(java.awt.Color.white,java.awt.Color.red,java.awt.Color.yellow,java.awt.Color.green) 
  val actions=new scala.collection.mutable.ListBuffer[(String,()=>Unit)]()
  
  var cellCallback={ (x:Int,y:Int) => println("Clicked on ("+x+","+y+")") }       
  var cells=Array.ofDim[Cell](colQtd,rowQtd)
  
	class Cell(text:String,val x:Int,val y:Int) extends Button(text) {
	  	import Swing._
	  	var currColor=0
	  
		preferredSize = ((cellSize,cellSize):Dimension)
		background = colors(0)		
		border= Swing.EmptyBorder //Swing.LineBorder(java.awt.Color.black,1)
		
		reactions += {
          case ButtonClicked(_) => cellCallback(x,y)
        }
		
		def switch= background={			
			val inc = if(currColor==(colors.length-1)) (1-colors.length) else 1
			currColor += inc
			colors(currColor)
		} 
	}
  
  def top = new MainFrame {
    title = "SwingApp"
        
    contents=new FlowPanel {
    	contents+=new BoxPanel(Orientation.Vertical) {
    
