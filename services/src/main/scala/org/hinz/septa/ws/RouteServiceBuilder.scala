package org.hinz.septa.ws

import org.hinz.septa._
import org.hinz.septa.gtfs._
import org.hinz.gis._

import cc.spray._
import cc.spray.http._
import cc.spray.http.MediaTypes._
import org.hinz.septa.DirectionFactory._

import akka.actor.{Actor, PoisonPill}
import Actor._

import net.liftweb.json._
import net.liftweb.json.Serialization._

import java.util.{Date,Calendar}

/**
 * The Route Service provides a restful interface to
 * SEPTA GTFS data
 *
 * All GTFS-based services are prefixed with /route
 *
 * Routes:
 * /route/routes ~ GET
 *       returns a list of all supported routes
 * /route/directions ~ GET ~ route: ID of the route
 *       returns a list of directions on a given route
 * /route/stations ~ GET ~ route: ID of the route, direction: name of the direction
 *       returns a list of all stations on a given route in the given direction
 *
 * @todo refactor for the following services example:
 * /routes/               - Return list of routes
 * /routes/44             - Returns directions on route 44
 * /routes/44/eastbound   - Returns stations on eastbound route 44
 */
trait RouteServiceBuilder extends ServiceBuilder {
  implicit val formats = Serialization.formats(NoTypeHints)

  import JSONP._

  val fixedDataLoader:FixedDataLoader

  val routeService = {
    pathPrefix("route") {
      path("routes") {
        parameter('callback ?) {
          callback =>
            get {
              _.complete(jsonp(callback, 
                               write(fixedDataLoader.getRoutes)))
            }
        }
        //@todo - refactor for restful url
      } ~ path("directions") {
        parameters('callback ?, 'route) {
          (callback, route) =>
            get {
              _.complete(jsonp(callback,
                               write(fixedDataLoader.getDirections(route).getOrElse(Map("error" -> "unexpected error - this route may not exist!")))))
            }
        }
      } ~ path("stations") {
        parameters('callback ?, 'route, 'direction) {
          (callback, route, direction) =>
            get {
              _.complete(jsonp(callback,
                               write(findStations(route,direction) match {
                                 case Some(x) => x
                                 case None => Map("error" -> "station not found")
                               })))
            }
        }
      }
    }
  }

  def findStations(route: String, dir: String):Option[List[GTFSLoader.Station]] =
    fixedDataLoader.getStations(route, dir).map(tupleList =>
      tupleList.map(_ match {
        case (routeid, desc) => GTFSLoader.stations.get(routeid)
        case _ => None
      }).flatten)

}
