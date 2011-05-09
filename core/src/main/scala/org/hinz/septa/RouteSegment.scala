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
                    start_ref: Double, end_ref: Double, recordedAt:Date, t: Double) {
  def toGInterval =
    GInterval(start_ref, end_ref, t, recordedAt)
}
