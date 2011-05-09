package org.hinz.septa

import java.awt.geom.Point2D

case class LatLon(lat:Double,lon:Double) {
	def toPoint2D = new Point2D.Double(lon,lat)
}

object RouteProcessor {
  
  def fudgeLat = 1.0/3600.0
  def fudgeLon = 1.0/3600.0

  /**
   * Find the smallest distance between the given route and
   * the given point
   *
   * If no distance less than a preset threshold can be found
   * this method returns None
   */
  def distanceOnRoute(route: List[RoutePoint], pt:LatLon):Option[Double] = {
    
    // Determine if any route point pair could contain this interval
    val minDist = 0.01

    val m =
      route.zip(route.tail).foldLeft((None:Option[(RoutePoint,RoutePoint)],minDist))((curmin,p) => {
        val p1 = p._1
        val p2 = p._2

        val maxlat = p1.lat.max(p2.lat)
        val minlat = p1.lat.min(p2.lat)
        val maxlon = p1.lon.max(p2.lon)
        val minlon = p1.lon.min(p2.lon)

        if (pt.lat >= minlat - fudgeLat  && pt.lat <= maxlat + fudgeLat &&
            pt.lon >= minlon - fudgeLon && pt.lon <= maxlon + fudgeLon) {
          println("Got here...")
          val minDist = GIS.minDistance((pt.lon,pt.lat),
                                      GIS.computeLine(p1.lon,p1.lat,p2.lon,p2.lat))

          if (minDist < curmin._2)
            (Some((p1,p2)), minDist)
          else
            curmin
        } else {
          curmin
        }
      })

    println(m._2)
   
    m._1.map( p => p._1.ref + GIS.distanceCalculator(p._1.lat,p._1.lon,pt.lat,pt.lon) )
  }

}
