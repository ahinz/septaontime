package org.hinz.septa

import org.hinz.gis._
import scala.xml._
import scala.io.Source

// Specify Integer.MIN_VALUE to flip the first item
case class RouteList(r: Route, order:List[Int])


object Routes {
  val route44 = List(route44_w_54,route44_w_ard, route44_e_dt1,route44_e_dt2)

  val route44_w_54 = 
    RouteList(Route(-1,"44","54th St S. of City Ave","",West), List(0,1,3,4,9,8,7,5))
  val route44_w_ard =
    RouteList(Route(-1,"44","Ardmore Station", "", West), List(0,1,3,4,9,8,7,-6,18,16,15))
  val route44_e_dt1 = 
    RouteList(Route(-1,"44","??", "Via Ardmore", East), List(-15,-16,-18,6,-7,10,-11,-4,-2,-1))
  val route44_e_dt2 =
    RouteList(Route(-1,"44","??", "Via 54th", East), List(-5,10,-11,-4,-2,-1))

  def processRoute(r: RouteList) = {
    val file = XML.loadString(Source.fromFile("/Users/ahinz/Downloads/" + r.r.shortname + ".kml").getLines.mkString("\n"))

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