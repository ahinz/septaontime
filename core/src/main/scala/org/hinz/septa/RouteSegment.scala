package org.hinz.septa

import java.util.Date
import org.hinz.gis.{Interval => GInterval}

/**
 * This file contains model definitions for the routes
 *
 * These definitions are loaded using the RouteLoader
 */

/**
 * Represents bus data scraped from the septa server
 *
 * @param id Database ID
 * @param route SEPTA Route (44,K,103, etc.)
 * @param lat
 * @param lon
 * @param time Time this data was recorded
 * @param block Block ID
 * @param bus Bus ID
 */
case class BusData(id: Int, route: String, lat: Double, lon:Double, time: Date, block: String, bus: String)

// Direction of the route
// db -> string stored in the database (n,s,...)
// septa -> septa string (SouthBound,NorthBound,...)
sealed class Direction(val db: String, septa:String)
case object North extends Direction("n","NorthBound")
case object South extends Direction("s","SouthBound")
case object East extends Direction("e","EastBound")
case object West extends Direction("w","WestBound")

/**
 * A Route is the parent of a map from lat/lon to a linear reference
 *
 * @param id Database id
 * @param shortname "32,K" (matches with BusData.route)
 * @param longname Represents the final destination of this bus ("54th/City","Via Wynewood")
 * @param direction The direction this route travels
 */
case class Route(id: Int, shortname: String, longname: String, direction:Direction)

/**
 * A Route Point represents a single point on a route
 */
case class RoutePoint(id: Int, route_id: Int, lat: Double, lon:Double, ref: Double)


case class Interval(id: Int, route_id:Int, bus_data_id1:Int, bus_data_id2:Int,
                    start: Double, end: Double, recordedAt:Date, time: Double) {

  def velocity = (end - start)/time

  def samePoints(i: Interval) = 
    U.dblCompare(i.start,start) && U.dblCompare(i.end,end)

  /**
   * Determine if this interval contains the given point
   *
   * @param d point to check
   * @return true if d is in [start,end]
   */
  def contains(d: Double, includeEndPoints:Boolean = true):Boolean =
    if (includeEndPoints)
      d >= start && d <= end
    else
      d > start && d < end

  /**
   * Split an interval at the given points
   */
  def split(ds: List[Double]):List[Interval] =
    splith(this, ds.sortWith(_ < _))

  private def splith(i:Interval, ds: List[Double]):List[Interval] = ds match {
    case Nil => List(i)
    case x::xs => {
      val splitInterval = i.split(x)
      splitInterval._1 :: i.splith(splitInterval._2, xs)
    }
  }

  /**
   * Split an interval at the given point
   * The ratio of distance to time remains the same
   *
   * If d is not in the interval an exception is thrown
   */
  def split(d: Double):(Interval,Interval) =
    if (contains(d)) 
      (Interval(id, route_id, bus_data_id1,bus_data_id2,start, d, recordedAt, ((d - start)/(end - start))*time),
       Interval(id, route_id, bus_data_id1,bus_data_id2,d, end,recordedAt, ((end -d)/(end - start))*time))
    else
      throw new Exception("Cannot split " + toString + " on " + d)

  override def toString = "[" + start + ", " + end + " (" + time +")]"


}

object U {
  def dblCompare(d1:Double,d2:Double,tol:Double = .0000000001) =
    math.abs(d1 - d2) < tol
}
