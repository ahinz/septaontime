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

case class PEstInterval(pts: List[LatLon], ival: EstInterval)

object Worker {
  implicit val formats = Serialization.formats(NoTypeHints)

  val ld = new RouteLoader("devdb.db")
  val e = new Estimator()

  def inner(start: Double, stop: Double, routeId: Int) = 
    ld.loadWithBuilder("select * from route_data where ref > " + start + " and ref < " + stop + " and route_id = " + routeId, ld.buildRoutePoint _).map(x => LatLon(x.lat,x.lon))

  def interpll(dist: Double, routeId: Int) = {
    val start_list = ld.loadWithBuilder("select * from route_data where ref > " + dist + " and route_id = " + routeId + " limit 1", ld.buildRoutePoint _)
    val stop_list = ld.loadWithBuilder("select * from route_data where ref < " + dist + " and route_id = " + routeId + " order by ref desc limit 1", ld.buildRoutePoint _)

    if (start_list.length == 0)
      LatLon(stop_list.head.lat, stop_list.head.lon)
    else if (stop_list.length == 0)
      LatLon(start_list.head.lat, start_list.head.lon)
    else {
      // Figure out pct dist
      val start = start_list.head
      val stop = stop_list.head
      val totalDist = stop.ref - start.ref
      val diff = dist - start.ref
      val pct = diff / totalDist

      LatLon(start.lat + (stop.lat - start.lat) * pct, start.lon + (stop.lon - start.lon) * pct)
    }
  }

  def fillOut(routeId: Int, ival: EstInterval):PEstInterval = {
    PEstInterval(List(List(interpll(ival.startDist, routeId)), inner(ival.startDist, ival.endDist, routeId), List(interpll(ival.endDist, routeId))).flatten, ival)
  }
  
  def fillOut(routeId: Int, ivals: List[EstInterval]):List[PEstInterval] = ivals.map(fillOut(routeId, _))

  def getIntervals(routeId: Int, segSizeKm: Double = 0.1, numSegs: Int = 3) = {
    // 24 hours of coverage
    val oneDay = 1000 * 60 * 60 * 24
    val today = new Date()
    val yesterday = new Date(today.getTime() - oneDay)
    val cal = Calendar.getInstance()

    cal.set(Calendar.HOUR_OF_DAY, 0)
    cal.set(Calendar.MINUTE, 0)
    
    val midnight_am = cal.getTime()

    cal.set(Calendar.HOUR_OF_DAY, 23)
    cal.set(Calendar.MINUTE, 59)
    val midnight_pm = cal.getTime()


    val ivals = ld.loadIntervals(routeId)
    val model = Model(routeId, segSizeKm, numSegs, (yesterday, Some(today)),  (midnight_am, midnight_pm), (0, ivals.map(_.end).max(Ordering[Double])))

    write(fillOut(routeId, e.estimateIntervals(model, ivals)))
  }
	

  // Clearly this code should be A LOT smarter...
  // TODO: Infer routes that this would work on
  // then use ALL POSSIBLE intervals from ALL POSSIBLE routes
  def getTime(start: LatLon, stop: LatLon, routeNum: String, d: Direction) = {
    // We are just going to fake a bus record for the starting stop
    val fakeBus = List(BusRecord(start.lat.toString,start.lon.toString,"","","",d.septa,"","0"))

    println("* About to esimate the time from " + start + " to " + stop + " on " + routeNum + " going " + d);
    val rts = ld.loadRoutes(Map("shortname" -> routeNum, "direction" -> d.db))

    println("* Found " + rts.length + " routes at this station")

    val intervals = rts.flatMap(r => ld.loadIntervals(r.id))

    // Get the time between the start (as a "bus") to the finish point
    // using all possible intervals from the route
    val pts:List[Either[String,List[BusEst]]] = rts.map(r => {
      println("  ** Processing route " + r)
      e.estimateNextBus(
        stop, 
        ld.loadRoutePoints(Map("route_id" -> r.id.toString)),
        fakeBus,
	Model.baseline(r.id),
        ld.loadIntervals(r.id))
    })

    println("* Done processing!")

    val finalEst:List[BusEst] = pts.filter(_.isRight).map(_.right.get).flatten

    // Remove duplciates:
    //TODO: This should be moved to the estimateNextBus method
    write(finalEst.groupBy(x => x.blockId + "." + x.busId).map(k => k._2.head).toList.sortWith(
      _.arrival.head.getTime > _.arrival.head.getTime) match {
      case x::xs => Map("arrival" -> x.arrival)
      case _ => Map[String,String]()
    })

  }

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
	Model.baseline(r.id),
        ld.loadIntervals(r.id))
    })

    println("* Done processing!")

    val finalEst:List[BusEst] = pts.filter(_.isRight).map(_.right.get).flatten
    
    // Remove duplciates:
    //TODO: This should be moved to the estimateNextBus method
    write(finalEst.groupBy(x => x.blockId + "." + x.busId).map(k => k._2.head).toList.sortWith(
      _.arrival.head.getTime < _.arrival.head.getTime))
    

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
    } ~ path("time" / "[^/]*".r / "[^/]*".r) {
      (route,direction) =>
      parameters('callback ?, 'lat1, 'lon1, 'lat2, 'lon2) {
        (callback, lat1, lon1, lat2, lon2) =>
          get {
            _.complete(
              jsonp(callback,
                    Worker.getTime(
                      LatLon(lat1.toDouble,lon1.toDouble),
                      LatLon(lat2.toDouble, lon2.toDouble),
                      route,
                      directionForString(direction).get)))
          }
      }
    }
  }

  val routeService = {
    path("interval") {
      parameters('callback ?, 'route_id, 'seg_size_km, 'num_segs) {
        (callback, route_id, seg_size_km, num_segs) =>
          get {
            _.complete(
              jsonp(callback,
                    Worker.getIntervals(route_id.toInt, seg_size_km.toDouble, num_segs.toInt)))
          }
      }
    } ~
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
