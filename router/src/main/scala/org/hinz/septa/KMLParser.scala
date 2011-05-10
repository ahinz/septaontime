package org.hinz.septa

import org.hinz.gis._

import java.sql.Connection
import java.sql.DriverManager
import java.sql.ResultSet
import java.sql.SQLException
import java.sql.Statement

import scala.xml._
import scala.io.Source

// Specify Integer.MIN_VALUE to flip the first item
case class RouteList(shortname: String, longname: String, desc: String, direction: String, order:List[Int])

object Routes {
  val route44_w_54 = 
    RouteList("44","54th St S. of City Ave","","w", List(0,1,3,4,9,8,7,5))
  val route44_w_ard =
    RouteList("44","Ardmore Station", "", "w", List(0,1,3,4,9,8,7,-6,18,16,15))
  val route44_e_dt1 = 
    RouteList("44","??", "Via Ardmore", "e", List(-15,-16,-18,6,-7,10,-11,-4,-2,-1))
  val route44_e_dt2 =
    RouteList("44","??", "Via 54th", "e", List(-5,10,-11,-4,-2,-1))

  def processRoute(r: RouteList) = {
    val file = XML.loadString(Source.fromFile("/Users/ahinz/Downloads/" + r.shortname + ".kml").getLines.mkString("\n"))

    val pts = coords(file)

    if (r.order == null)
      pts
    else
      List(r.order.map(v =>
        if (v >= 0) pts(v)
        else if (v == Integer.MIN_VALUE) pts(0).reverse
        else pts(-v).reverse).flatten)
  }

  def coords(xml:NodeSeq) = {
    (xml \\ "coordinates").map(_.text.split(" ").toList.map(llpair => {
      val ll = llpair.split(",")
      if (ll.length == 3) {
        Some((ll(1).toDouble, ll(0).toDouble))
      } else {
        None
      }
    }).flatMap(x => x)).toList
  }

}

class DBWriter(db:String) {
  Class.forName("org.sqlite.JDBC")

  def writeSinglePoint(statement:Statement, rid:Int,  lat1: Double, lon1: Double, dist: Double) = {
    val stmt = "insert into route_data (route_id, lat, lon, ref) values (" +
                            rid + ", " + lat1 + ", " + lon1 + ", " + dist + ")"
    println(stmt)
   // statement.executeUpdate(stmt)
  }

  def writePoints(routeid:Int, pts: List[(Double,Double)]) = {
    val connection = DriverManager.getConnection(
      "jdbc:sqlite:devdb.db")
    val statement = connection.createStatement();
    
    addDistanceToPts(pts).map(x => writeSinglePoint(statement, routeid, x._1,x._2,x._3))
  }

  def addDistanceToPts(l: List[(Double,Double)], acc: List[(Double,Double,Double)]=Nil):List[(Double,Double,Double)] = l match {
    case Nil => acc.reverse
    case x::xs => 
      if (acc.length == 0)
        addDistanceToPts(xs, (x._1,x._2,0.0) :: acc)
      else
        addDistanceToPts(xs, (x._1,x._2,acc.head._3 + 
                              GIS.distanceCalculator(x._1,x._2,acc.head._1,acc.head._2)) :: acc)
  }
        
}



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

object HelloWorld extends SimpleSwingApplication {
  def top = new MainFrame {
    size = new java.awt.Dimension(1000,600)
    preferredSize = new Dimension(1000,600)
    title = "Hello, World!"
    contents = new GISPanel(Routes.processRoute(Routes.route44_e_dt2))
  }
}

