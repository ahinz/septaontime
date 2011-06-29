package org.hinz.septa

import java.util.Date
import org.hinz.gis._

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

/**
 * Information about when a block rotates from one route to another
 * (i.e. from '44 to Ardmore' to '44 to 5th and Market')
 */
case class BlockRotate(block: String, time: Date, fromRoute: String, toRoute: String)

/**
 * Information about a given block
 *
 * @param block the block # for this block
 * @param route the route number that this block drives on
 * @param start the earliest this block has been seen
 * @param stop the latest this block has been seen
 */
case class BlockInfo(block: String, route: String, start: Date, stop:Date, rotations:List[BlockRotate])

// Direction of the route
// db -> string stored in the database (n,s,...)
// septa -> septa string (SouthBound,NorthBound,...)
object DirectionFactory {
  def directionForString(db:String):Option[Direction] = db match {
    case "n" => Some(North)
    case "s" => Some(South)
    case "e" => Some(East)
    case "w" => Some(West)
    case "Northbound" => Some(North)
    case "Southbound" => Some(South)
    case "Eastbound" => Some(East)
    case "Westbound" => Some(West)
    case _ => None
  }
}

sealed class Direction(val db: String, val septa:String)
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
case class Route(id: Int, shortname: String, longname: String, desc: String, direction:Direction)

/**
 * A Route Point represents a single point on a route
 */
case class RoutePoint(id: Int, route_id: Int, lat: Double, lon:Double, ref: Double) {
  def distanceTo(tgtPt: LatLon) =
      ref + GIS.distanceCalculator(lat, lon, tgtPt.lat, tgtPt.lon)
}


case class Interval(id: Int, route_id:Int, bus_data_id1:Int, bus_data_id2:Int,
                    start: Double, end: Double, recordedAt:Date, time: Double) {

  def velocity = (end - start)/time

  def overlaps(i: Interval):Boolean =
    overlaps(i.start, i.end)

  def overlaps(istart: Double, iend: Double):Boolean =
    (istart >= start && istart <= end) ||
    (iend >= start && iend <= end) ||
    (istart <= start && iend >= end)

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

  override def toString = "[" + start + ", " + end + " (" + time +" / " + recordedAt + ")]"


}

object U {
  def dblCompare(d1:Double,d2:Double,tol:Double = .0000000001) =
    math.abs(d1 - d2) < tol
}
