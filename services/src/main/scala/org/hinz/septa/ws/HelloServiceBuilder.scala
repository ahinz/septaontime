package org.hinz.septa.ws

import org.hinz.septa._
import org.hinz.gis._

import cc.spray._
import org.hinz.septa.DirectionFactory._

import akka.actor.{Actor, PoisonPill}
import Actor._

import net.liftweb.json._
import net.liftweb.json.Serialization._

object Worker {
  implicit val formats = Serialization.formats(NoTypeHints)

  //TODO: Make this into an actor?
  //This can all be done async
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
    val pts:List[Either[String,List[BusEst]]] = rts.map(r => e.estimateNextBus(
      station, 
      ld.loadRoutePoints(Map("route_id" -> r.id.toString)),
      live,
      ld.loadIntervals(r.id)))

    val finalEst:List[BusEst] = pts.filter(_.isRight).map(_.right.get).flatten

    write(finalEst)
  }

}
      

trait HelloServiceBuilder extends ServiceBuilder {
  
  /**
   * To get an estimate we need to know:
   * - Lat/lon of the station
   * - Route we want to query (route #)
   */
  val helloService = {
    path("next") { 
      parameters('lat,'lon,'route, 'direction) {
        (lat, lon, route, direction) =>
          get { 
            _.complete(Worker.getEstimates(LatLon(lat.toDouble, lon.toDouble), route, directionForString(direction).get)) }
      }
    }
    
  }

  
}
