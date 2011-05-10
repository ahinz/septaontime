package org.hinz.gis;

import scala.xml._
import scala.io.Source

import org.hinz.septa._

import swing._
import scala.swing.event._
import java.awt.geom._
import java.awt.Color
import java.util.Date


abstract class Renderer {
  def render(g:Graphics2D, transform: AffineTransform, parent: Panel):Unit
}

class IntervalRenderer(var ivals:List[List[Interval]]) extends Renderer {
  override def render(g:Graphics2D, transform: AffineTransform, parent: Panel):Unit = {
    var y = 10
    
    ivals.map(intervals => {
      intervals.map(interval => {
        val startAt = interval.start
        val endAt = interval.end

        // Just want to transform X values:
        val tStart = transform.transform(new Point2D.Double(startAt, 0.0), null).getX
        val tEnd = transform.transform(new Point2D.Double(endAt, 0.0), null).getX

        g.drawLine(tStart.toInt, y, tEnd.toInt, y)

        y += 3
        if (y > 530) y = 10
      })

      y += 20
    })
  }
}
      

class PlotterPanel(renderers:List[Renderer]) extends Panel {
 
  val boundsY = (0.0, 200.0)
  val boundsX = (0.0, 26.0)

  val xScale:Double = 950.0 
  val yScale:Double = 550.0 

  val scaleTransform = AffineTransform.getScaleInstance(
    xScale / (boundsX._1 - boundsX._2),
    yScale/ (boundsY._2 - boundsY._1))

  val translateTransform = AffineTransform.getTranslateInstance(
    - boundsX._2,  - boundsY._1)

  translateTransform.preConcatenate(scaleTransform)

  val transform = translateTransform

  listenTo(mouse.clicks)

  def d(x1: Double, y1:Double, x2:Double, y2: Double) =
    math.sqrt((x1 - x2)*(x1 - x2) + (y1 - y2)*(y1-y2))

  def doClick(p: Point) = {
  }

  reactions += {
    case MouseClicked(_,p,_,_,_) => doClick(p)
  }

  override def paint(g: Graphics2D) = {
    renderers.map(_.render(g,transform,this))
  }
}


object HelloWorld extends SimpleSwingApplication {

  val db = "devdb.db"
    
  val ld = new RouteLoader(db)

  val ivals = ld.loadIntervals(1,0,30)

  // Let's try to make some guesses:
  val busdata = LiveDataLoader.getMostRecentLiveData("23")

  

  // Let's try to make a guess!!
  val blat = 40.010868
  val blon = -75.151306

  var slat = 40.036234
  val slon = -75.175208
  var sref = 16.8804151618758

  val r = new RouteLoader(db)
  val routepts = r.loadRoutePoints(Map("route_id" -> "1"))
  println("Loaded " + routepts.length + " points!")
  val buses = LiveDataLoader.getMostRecentLiveData("23").filter(_.Direction == "NorthBound")

  println(new Estimator().estimateNextBus(LatLon(slat,slon), routepts,buses, ivals).mkString("\n"))


  def top = new MainFrame {
    size = new java.awt.Dimension(1000,600)
    preferredSize = new Dimension(1000,600)
    title = "Hello, World!"
    contents = new PlotterPanel(List(new IntervalRenderer(List(ivals))))
  }
}
