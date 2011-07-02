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
    path("routes") {
      parameter('callback ?) {
        callback =>
          get {
            _.complete(jsonp(callback, 
                             write(fixedDataLoader.getRoutes)))
          }
      }
    } ~ pathPrefix("routes" / "[^/]*".r) { route =>
      path("") {
        parameter('callback ?) {
          (callback) =>
            get(ctxt => { 
              fixedDataLoader.getDirections(route) match {
                case Some(dirs) => ctxt.complete(
                  jsonp(callback, write(dirs)))
                case None => ctxt.fail(HttpStatusCodes.NotFound)
              }
            })
        }
      } ~ pathPrefix("[^/]*".r) { direction =>
        parameter('callback ?) {
          (callback) =>
            get(ctxt => {
              fixedDataLoader.findStations(route, direction) match {
                case Some(stations) => ctxt.complete(jsonp(callback,write(stations)))
                case None => ctxt.fail(HttpStatusCodes.NotFound)
              }
            })
        }
      }
    }
  }
}
