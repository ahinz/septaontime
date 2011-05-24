package org.hinz.septa

import swing._
import scala.swing.event._
import java.awt.geom._
import java.awt.Color

class GISPanel(pts:List[List[(Double,Double)]]) extends Panel {
 
  var sel:List[(Double,Double)] = null

  val lats = pts.flatMap(_.map(_._1))
  val lons = pts.flatMap(_.map(_._2))

  val boundsLat = (lats.max, lats.min)
  val boundsLon = (lons.max, lons.min)

  val xScale:Double = 950.0 //size.width
  val yScale:Double = 550.0 //size.height

  val scaleTransform = AffineTransform.getScaleInstance(
    xScale / (boundsLon._1 - boundsLon._2),
    yScale/ (boundsLat._2 - boundsLat._1))

  val translateTransform = AffineTransform.getTranslateInstance(
    - boundsLon._2,  - boundsLat._1)

  translateTransform.preConcatenate(scaleTransform)

  val transform = translateTransform

  listenTo(mouse.clicks)

  def d(x1: Double, y1:Double, x2:Double, y2: Double) =
    math.sqrt((x1 - x2)*(x1 - x2) + (y1 - y2)*(y1-y2))

  def doClick(p: Point) = {
    val latlon = transform.inverseTransform(p, null)

    // Find nearest point
    val minpt = pts.flatMap(x => x.map(a => (a,d(a._2,a._1, latlon.getX, latlon.getY)))).sortWith(_._2 < _._2).head._1

    // Find that row
    sel = pts.filter(_.contains(minpt)).head

    // Output the index
    println("Selected index: " + pts.indexOf(sel))

    repaint()
  }

  reactions += {
    case MouseClicked(_,p,_,_,_) => doClick(p)
  }

  override def paint(g: Graphics2D) = {

    val colors = List(Color.BLUE,Color.GREEN)

    var k = 0

    // 0,1,3,4,9,8,7,5
    val pts2:List[List[(Double,Double)]] = pts
//      List(List(pts(0),pts(1),pts(3),pts(4),pts(9),pts(8),pts(7),pts(5)).flatten) //.reverse,pts(5).reverse,pts(6)).flatten)

    var ptx:Point2D = null

    g.setColor(Color.WHITE)
    g.fillRect(0,0,xScale.toInt,yScale.toInt)

    pts2.map( p => {

      if (p == sel) {
        g.setColor(Color.RED)
      } else {
        g.setColor(colors(k % colors.length))
        k += 1
      }

      p.zip(p.tail).map(p => {
        val px = new Point2D.Double(p._1._1,p._1._2);
        val p1:Point2D = transform.transform(new Point2D.Double(p._1._2,p._1._1), null)
        val p2:Point2D = transform.transform(new Point2D.Double(p._2._2,p._2._1), null)

        g.drawLine(p1.getX.toInt, p1.getY.toInt, p2.getX.toInt, p2.getY.toInt)
      })

      val pt = p.last
      val p1:Point2D = transform.transform(new Point2D.Double(pt._1,pt._2), null)
     
      g.fillOval(p1.getX.toInt, p1.getY.toInt, 5, 5)

      if (p == sel) {
        ptx = p1
      }

      val pt2 = p.last
      val p2:Point2D = transform.transform(new Point2D.Double(pt2._1,pt2._2), null)

     g.fillRect(p2.getX.toInt,p2.getY.toInt,5,5)
    });

    g.setColor(Color.RED)
    if (ptx != null) {
      g.fillOval(ptx.getX.toInt, ptx.getY.toInt, 10,10)
    }
  }
}





