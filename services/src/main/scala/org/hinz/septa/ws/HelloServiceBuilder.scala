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

object Worker {
  implicit val formats = Serialization.formats(NoTypeHints)

  val ld = new RouteLoader("devdb.db")
  val e = new Estimator()

  def getEstimates(station: LatLon, routeNum: String, d: Direction) = {
    println("* About to esimate next bus on route " + routeNum + " leaving from " + station)
    val rts = ld.loadRoutes(Map("shortname" -> routeNum, "direction" -> d.db))

    println("* Found " + rts.length + " routes at this station")

    // Grab live data for this route
    val live = LiveDataLoader.getMostRecentLiveData(routeNum).filter(_.Direction == d.septa)
    println("* Found " + live.length + " possible enroute buses")
    
    
    // Grab estimates for each route:
    val pts:List[Either[String,List[BusEst]]] = rts.map(r => {
      println("  ** Processing route " + r)
      e.estimateNextBus(
        station, 
        ld.loadRoutePoints(Map("route_id" -> r.id.toString)),
        live,
        ld.loadIntervals(r.id))
    })

    println("* Done processing!")

    val finalEst:List[BusEst] = pts.filter(_.isRight).map(_.right.get).flatten
    
    // Remove duplciates:
    //TODO: This should be moved to the estimateNextBus method
    write(finalEst.groupBy(x => x.blockId + "." + x.busId).map(k => k._2.head).toList.sortWith(
      _.arrival.getTime < _.arrival.getTime))
    

//    write(finalEst)
  }

}
      
object JSONP {
  def apply(callback: String, body: String) = 
    jsonp(callback, body)

  def wrapJSONP(callback: String, body: String) =
    if (callback != null && callback.length > 0) callback + "(" + body + ")"
    else body

  def jsonp(callback: String, body: String) =
    HttpContent(
      ContentType(`application/json`),
      wrapJSONP(callback, body))
}

trait HelloServiceBuilder extends ServiceBuilder {
  implicit val formats = Serialization.formats(NoTypeHints)
  
  import JSONP._

  /**
   * To get an estimate we need to know:
   * - Lat/lon of the station
   * - Route we want to query (route #)
   */
  val nextBusService = {
    path("next") { 
      parameters('callback ?, 'lat,'lon,'route, 'direction) {
        (callback, lat, lon, route, direction) =>
          get { 
            _.complete(
              jsonp(callback,
                   Worker.getEstimates(LatLon(lat.toDouble, lon.toDouble), route, directionForString(direction).get)))
          }
      }
    }
  }

  val routeService = {

    pathPrefix("route") {
      path("routes") {
        parameter('callback ?) {
          callback =>
            get {
              _.complete(jsonp(callback, 
                               write(FixedDataLoader.getRoutes)))
            }
        }
      } ~ path("directions") {
        parameters('callback ?, 'route) {
          (callback, route) =>
            get {
              _.complete(jsonp(callback,
                               write(FixedDataLoader.getDirections(route).getOrElse(Map("error" -> "unexpected error - this route may not exist!")))))
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
    FixedDataLoader.getStations(route, dir).map(tupleList =>
      tupleList.map(_ match {
        case (routeid, desc) => GTFSLoader.stations.get(routeid)
        case _ => None
      }).flatten)

      
  
}
