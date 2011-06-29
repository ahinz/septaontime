package org.hinz.septa

case class Bounds(latmin: Double, lonmin:Double, latmax:Double, lonmax:Double) {
  def contains(lat:Double, lon:Double) =
    lat >= latmin && lat <= latmax &&
    lon >= lonmin && lon <= lonmax

  def expand(lat:Double, lon:Double) =
    Bounds(latmin - lat, lonmin - lon, latmax + lat, lonmax + lon)
}

/**
 * Tools to use with latitude/longitude and interpolating distances
 */
object GIS {
  var expansionFactor = 1.0/3600.0

  import math._

  private def deg2rad(x:Double) = x / 180.0 * java.lang.Math.PI

  /**
   * Distance between two points on a plane (used mostly for testing)
   */
  val flatDistanceCalculator = (x1:Double,y1:Double,x2:Double,y2:Double) => 
    math.sqrt((x1 - x2)*(x1 - x2) + (y1 - y2)*(y1 - y2))
  
  /**
   * Distance between two points on a sphere
   *
   * @param lat1
   * @param lon1
   * @param lat2
   * @param lon2
   *
   * @return distance between two points in kilometers
   */
  val sphereDistanceCalculator = (lat1:Double,lon1:Double,lat2:Double,lon2:Double) => {
    // from http://www.movable-type.co.uk/scripts/latlong.html

    val R = 6371.0 // km
    val dLat = deg2rad(lat2-lat1)
    val dLon = deg2rad(lon2-lon1) 
    val a = sin(dLat/2.0) * sin(dLat/2.0) +
            cos(deg2rad(lat1)) * cos(deg2rad(lat2)) * 
            sin(dLon/2.0) * sin(dLon/2.0) 
    val c = 2 * atan2(sqrt(a), sqrt(1.0-a)) 
    val d = R * c

    d
  }

  /**
   * Plugable distance calculator (note that the sphereDistanceCalculator can be
   * used here for km or you can multiply by a factor to get other units)
   */
  var distanceCalculator:((Double,Double,Double,Double) => Double) = sphereDistanceCalculator

  /**
   * Create a bounds from two latitude/longitude points
   */
  def createBounds(lat1:Double, lon1:Double, lat2:Double, lon2:Double) =
    Bounds(lat1 max lat2, lon1 max lon2, lat1 min lat2, lon1 min lon2)

  /**
   * Determine the minimum distance between a point and a line on a plane
   * (note - we use this even on a sphere since the latitude/longitude pairs
   * are close [around 20 meters] enough that we can ignore the ill effects)
   *
   * @param p Point
   * @param l (slope,y-intercept)
   * @param m Slope of line
   * @param b y-intercept of line
   */
  def minDistance(p:(Double,Double), l:(Double,Double)):Double = {
    val m = l._1
    val b = l._2

    // Y-intercept of intercepting line
    val m2 = -1.0/m
    val b2 = p._2 - m2*p._1

    // Special case: same line
    if ((m - m2) == 0)
      return 0.0

    // Determine intersecting point:
    val x = (b2 - b)/(m - m2)
    val y = m*x + b

    // Find the distance between these two points
    flatDistanceCalculator(p._1,p._2,x,y)
  }

  /**
   * Given two points compute the slope and y-intercept
   * generated by connecting the two points with a straight line
   * in a plane
   *
   * @param x1
   * @param y1
   * @param x2
   * @param y2
   * @return (Slope,Y-Intercept)
   */
  def computeLine(x1:Double,y1:Double,x2:Double,y2:Double) = {
    val m = (y1 - y2)/(x1 - x2)
    val b = y1 - m*x1
    (m,b)
  }


}
