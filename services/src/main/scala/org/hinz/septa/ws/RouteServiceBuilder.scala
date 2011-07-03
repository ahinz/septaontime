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
import JSONP._

/**
 * Station service provides the actual estimates
 * 
 * Routes:
 * /station/1492                 - Return info about station #1492
 * /station/1492/bus/44/east     - Return bus estimates for route 44 at stop #1492 going east
 * /station/1492/to/2522/west    - Return bus estimates from station 1492 to 2522 going west
 *
 * For the last two you can specify the following additional model params [default]:
 * segmentSizeKm              - Size of the summary segment (must be > 0.1) [0.1]
 * startDate                  - Start date in milliseconds for interp [24 hours ago]
 * endDate                    - End date in milliseconds for interp ['now']
 * startTime                  - Start time (hh:mm) [midnight]
 * endTime                    - End time (hh:mm) [midnight - 1s]
 */ 
trait StationServiceBuilder extends ServiceBuilder {
  implicit val stationFormats = Serialization.formats(NoTypeHints)

  val fixedDataLoader:FixedDataLoader
  val loader: RouteLoader
  val estimator: Estimator

  def parseTime(time: String) = {
    val c = Calendar.getInstance()
    val parts = time.split(":")
    c.set(Calendar.HOUR_OF_DAY, parts(0).toInt)
    c.set(Calendar.MINUTE, parts(1).toInt)
    c.getTime()
  }

  val stationService = {
    pathPrefix("station" / "\\d+".r) { stationId =>
      path("") {
        parameter('callback ?) {
          callback =>
            get(ctxt => {
              loader.loadStation(stationId.toInt) match {
                case Nil => ctxt.fail(HttpStatusCodes.NotFound)
                case st::t => ctxt.complete(jsonp(callback,
                                               write(st)))
              }
            })
        }
      } ~
      pathPrefix("to" / "\\d+".r / "[^/]+".r) { (endStation, direction) =>
        path("") {
          parameter('callback ?) {
            callback => 
              parameters('segmentSizeKm ? "0.1", 
                         'startDate ? (new Date().getTime() - 1000*60*60*24).toString,
                         'endDate ? new Date().getTime().toString,
                         'startTime ? "00:00",
                         'endTime ? "23:59") {
                (segmentSizeKm, startDate, endDate, startTime, endTime) =>
                  get(ctxt => {
                    val d = DirectionFactory.directionForString(direction).get

                    val station1 = loader.loadStation(stationId.toInt)                    
                    val station2 = loader.loadStation(endStation.toInt)

                    val possibleRoutes1 = fixedDataLoader.routesAtStation(stationId)
                    val possibleRoutes2 = fixedDataLoader.routesAtStation(endStation)

                    val possibleRoutes = possibleRoutes1.intersect(possibleRoutes2)

                    println("* Found possible route numbers: " + possibleRoutes)

                    if (station1 == Nil || station2 == Nil) {
                      ctxt.fail(HttpStatusCodes.NotFound)
                    } else {
                      val routes = loader.loadRoutes(possibleRoutes, d.db)
                      
                      println("* Found " + routes.length + " routes at this station")
                      println("\t* Picking 1st route: " + routes.head)

                      val tgtRoute = routes.head

                      val model = Model(tgtRoute.id, segmentSizeKm.toDouble, 10,
                                        (new Date(startDate.toLong),
                                         Some(new Date(endDate.toLong))),
                                        (parseTime(startTime),
                                         parseTime(endTime)),
                                        null)                       

                      val routePoints = loader.loadRoutePoints(
                        Map("route_id" -> tgtRoute.id.toString))

                      val intervals = loader.loadIntervals(tgtRoute.id)

                      val est = estimator.estimateNextBus(
                        LatLon(station1.head.lat.toDouble, 
                               station1.head.lon.toDouble),
                        routePoints,
                        List(BusRecord(
                          station2.head.lat,
                          station2.head.lon,
                          "", "", "", "", "", "0")),
                        List(model),
                        intervals)

                      println("Estimate: " + est)
                      ctxt.complete(jsonp(callback,
                                          write(est.map(ivalList =>
                                            ivalList.map(busEst => busEst.arrival.head)).getOrElse(List()))))
                    }
                  })
              }
          }
        }
      } ~
      pathPrefix("bus" / "\\d+".r / "[^/]+".r) { (route,direction) =>
        path("") {
          parameter('callback ?) {
            callback => 
              parameters('segmentSizeKm ? "0.1", 
                         'startDate ? (new Date().getTime() - 1000*60*60*24).toString,
                         'endDate ? new Date().getTime().toString,
                         'startTime ?,
                         'endTime ?) {
                (segmentSizeKm, startDate, endDate, startTime, endTime) =>
                  get(ctxt => {
                    val stations = loader.loadStation(stationId.toInt)                    

                    if (stations == Nil) {
                      println("Invalid Station")
                      ctxt.fail(HttpStatusCodes.NotFound)
                    } else {
                      // Extract the direction [ex: eastbound, east, e]
                      val dOpt = DirectionFactory.directionForString(direction)
                      val station = stations.head
                      val stationLatLon = LatLon(station.lat.toDouble, station.lon.toDouble)
                      
                      if (dOpt.isDefined) {
                        val d = dOpt.get
                        
                        // Determine possible route matches
                        val routes = loader.loadRoutes(
                          Map("shortname" -> route,
                              "direction" -> d.db))                      
                        
                        println("* Found " + routes.length + " routes at this station")
                        
                        val busses = LiveDataLoader.getMostRecentLiveData(route).filter(
                          _.Direction == d.septa)

                        val busEsts:List[Option[List[BusEst]]] = routes.map(route => {
                          println("\t* Procssing " + route)
                          val routePoints = loader.loadRoutePoints(
                            Map("route_id" -> route.id.toString))

                          val intervals = loader.loadIntervals(route.id)
                          val model = Model(route.id, segmentSizeKm.toDouble, 10,
                                            (new Date(startDate.toLong),
                                             Some(new Date(endDate.toLong))),
                                            (parseTime(startTime),
                                             parseTime(endTime)),
                                            null) 
                          
                          estimator.estimateNextBus(stationLatLon,
                                                    routePoints,
                                                    busses,
                                                    List(model),
                                                    intervals)
                        })
                          
                        println("* Results: " + busEsts)

                        ctxt.complete(
                          jsonp(callback, write("hmmmm")))
                        
                      } else {
                        println("invalid direction")
                        ctxt.fail(HttpStatusCodes.NotFound)
                      }
                    }
                  })
              }
          }
        }
                                              }
    }
  }
}


//@todo - psuedo routes [from databaase] maybe an /intervals/x or /points/x

/**
 * The Route Service provides a restful interface to
 * SEPTA GTFS data
 *
 * All GTFS-based services are prefixed with /routes
 *
 * Routes:
 * /routes/               - Return list of routes
 * /routes/44             - Returns directions on route 44
 * /routes/44/eastbound   - Returns stations on eastbound route 44
 */
trait RouteServiceBuilder extends ServiceBuilder {
  implicit val formats = Serialization.formats(NoTypeHints)

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
