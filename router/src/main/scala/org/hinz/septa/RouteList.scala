package org.hinz.septa

import org.hinz.gis._
import scala.xml._
import scala.io.Source

// Specify Integer.MIN_VALUE to flip the first item
case class RouteList(r: Route, order:List[Int])

object RouteCreator {
  def createRoute(r: RouteList, ld: RouteLoader) = {
    if (ld.loadRoutes(
	Map("longname" -> r.r.longname,
		"desc" -> r.r.desc)).length > 0) {
      println("  -> Skipping " + r + " (already in db)")
    } else {
      val dbRoute = ld.createRoute(r.r)
      println("  -> Inserted route " + dbRoute)
      
      val pts = Routes.processRoute(r,Routes.loadFromXML(r.r)).head
      
      createRouteData(dbRoute, pts).map(ld.createRouteData(_))
    }
  }

  def createRouteData(r: Route, l: List[(Double,Double)], acc: List[RoutePoint]=Nil):List[RoutePoint] = l match {
    case Nil => acc.reverse
    case x::xs => 
      if (acc.length == 0)
        createRouteData(r, xs, RoutePoint(-1,r.id,x._1,x._2,0) :: acc)
      else
        createRouteData(r, xs, RoutePoint(-1,r.id,x._1,x._2,acc.head.ref + 
                              GIS.distanceCalculator(x._1,x._2,acc.head.lat,acc.head.lon)) :: acc)
  }
}

/**
 * This object contains the static mappings from route KML files to
 * a single stream of connected datapoints
 *
 * Each route is described by a series of indicies. To reverse the order
 * of points mark the index as negative. If the first index (index 0) should
 * be reversed it should be marked as Integer.MIN_VALUE
 *
 * Each "section" of a normal route should be extended for the entire normal
 * route. For example, if a route went from center city to the main line and then
 * split to Cynwyd and to Bryn Mawr, each of those should be a route from the terminus
 * all of the way back downtown, duplicating the overlapped section
 */
object Routes {
  val route44_w_54 = 
    RouteList(Route(-1,"44","54th St S. of City Ave","",West), List(0,1,3,4,9,8,7,5))
  val route44_w_ard =
    RouteList(Route(-1,"44","Ardmore Station", "", West), List(0,1,3,4,9,8,7,-6,18,16,15))
  val route44_e_dt1 = 
    RouteList(Route(-1,"44","5th & Market St.", "Via Ardmore", East), List(-15,-16,-18,6,-7,10,-11,-4,-2,-1))
  val route44_e_dt2 =
    RouteList(Route(-1,"44","5th & Market St.", "Via 54th", East), List(-5,10,-11,-4,-2,-1))
  val route44_w_glad = 
    RouteList(Route(-1,"44","Gladwyne","",West), List(0,1,3,4,9,19,21,22))
  val route44_e_dt3 =
    RouteList(Route(-1,"44","5th & Market St.", "Via Gladwyne", East), List(-22,-21,-19,-11,-4,-2,-1))

  val route44 = List(
    route44_w_54,
    route44_w_ard, 
    route44_w_glad,
    route44_e_dt1,
    route44_e_dt2,
    route44_e_dt3)


  /**
   * Load KML file from a local data store
   *
   * @param r the Route to load
   * @return a List of segments
   */
  def loadFromXML(r: Route) = {
    val file = XML.loadString(Source.fromFile("/Users/ahinz/Downloads/" + r.shortname + ".kml").getLines.mkString("\n"))

    coords(file)
  }

  /**
   * Process the route list, based on order
   *
   * If order is NULL then pts is returned unaltered
   * 
   * @param r the route list
   * @return a single element list containing a list with the final points
   */
  def processRoute(r: RouteList, pts:List[List[(Double,Double)]]):List[List[(Double,Double)]] = 
    if (r.order == null)
      pts
    else
      List(r.order.map(v =>
        if (v >= 0) pts(v)
        else if (v == Integer.MIN_VALUE) pts(0).reverse
        else pts(-v).reverse).flatten)


  /**
   * Parse KML document
   *
   * @param xml XML parsed kml document
   * @return List of line segments where each segment is a list of poitns
   */
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
