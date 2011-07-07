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

import java.text._
import java.util.{Date,Calendar}
import JSONP._

trait ServiceUtils {
  def parseTime(time: Double) =  {
    val c = Calendar.getInstance()
    val hours = time.toInt
    val minutesFrac = time.toDouble - hours
    val minutes = minutesFrac * 60.0
    
    c.set(Calendar.HOUR_OF_DAY, hours)
    c.set(Calendar.MINUTE, minutes.toInt)
    c.getTime()      
  }
}

/**
 * Station service provides the actual estimates
 * 
 * Routes:
 * /station/1492                  - Return info about station #1492
 * /station/1492/bus/44/east      - Return bus estimates for route 44 at stop #1492 going east (addl model params apply)
 * /station/1492/to/2522/west     - Return bus estimates from station 1492 to 2522 going west (addl model params apply)
 * /station/1492/schedule/44/east?date={date} - Return the schedule for busses on route 44 arriving at station 1492 on date going eastbound
 *
 * Additional Model Params [default]:
 * segmentSizeKm              - Size of the summary segment (must be > 0.1) [0.1]
 * startDate                  - Start date in milliseconds for interp [24 hours ago]
 * endDate                    - End date in milliseconds for interp ['now']
 * startTime                  - Start time (decimal hours) [now - 2h]
 * endTime                    - End time (decimal hours) [now]
 *
 * In addition to the above parameters, the following parameters can be used for
 * over-time analysis:
 * seriesTimeIncrement        - Increment for time [in decimal hours]
 * numberOfRuns               - Number of runs (must be <= 8) [1]
 */
trait StationServiceBuilder extends ServiceBuilder with ServiceUtils {
  implicit val stationFormats = Serialization.formats(NoTypeHints)

  val fixedDataLoader:FixedDataLoader
  val loader: RouteLoader
  val estimator: Estimator

  def curTime() = {
    val c = Calendar.getInstance()
    c.setTime(new Date())

    c.get(Calendar.HOUR_OF_DAY).toDouble + c.get(Calendar.MINUTE).toDouble/60.0
  }

  def expandModel(model: Model, timeStart: Double, timeSpan: Double, timeInc: Double, numModels: Int):List[Model] = 
    for(j <- (0 until numModels).toList)
      yield(Model(model.route,
                  model.summarySegmentSizeKm,
                  model.maxNumberOfIntervals,
                  model.dateRange,
                  (parseTime(timeStart + timeInc*j.toDouble),
                   parseTime(timeStart + timeInc*j.toDouble + timeSpan)),
                  model.distRange))

  def interpDate(endDate: Date, offsetSeconds: Double, startDist: Double, endDist: Double, dist: Double) = {
    val distPct = (dist - startDist) / (endDist - startDist)
    val startDate = endDate.getTime - offsetSeconds * 1000
    
    new Date((startDate + (endDate.getTime - startDate)*distPct).toLong)
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
      path("routes") {
	parameter('callback ?) {
	  callback =>
	    get(ctxt => {
	      ctxt.complete(jsonp(callback, write(loader.routesForStation(stationId.toInt))))
	    })
	}
      } ~		  
      pathPrefix("schedule" / "\\d+".r / "[^/]*".r) { (route, direction) =>
	path("") {
	  parameters('callback ?, 'date) {
	    (callback, date) =>
	      get(ctxt => {
		val d = DirectionFactory.directionForString(direction).get
		
		val station = loader.loadStation(stationId.toInt) 
		val routes = loader.loadRoutes(Map("shortname" -> route, "direction" -> d.db))
		
		val selectedRoute = routes.head
		
		println("* Using route " + selectedRoute)
		
		val routepts = loader.loadRoutePoints(
                  Map("route_id" -> selectedRoute.id.toString))
		
		val stationLatLon = LatLon(station.head.lat.toDouble,station.head.lon.toDouble)
                val stationDist = estimator.nearestPointOnRoute(routepts,stationLatLon).get.distanceTo(stationLatLon)
		
		val startDate = new Date(date.toLong)
		val endDate = new Date(startDate.getTime + 1000*60*60*24 - 1)
		
		println("* Intervals ranging from " + startDate + " to " + endDate)
		
		val format = new SimpleDateFormat("yyyy-MM-dd")
		
		val whereDist = "route_id = " + selectedRoute.id + " AND " + 
		  stationDist + " > start_ref AND " + stationDist + " < end_ref";
		
		val ivals = loader.loadIntervalsWhere(whereDist).filter(
		  ival =>
		    ival.recordedAt.getTime < endDate.getTime && 
		    ival.recordedAt.getTime > startDate.getTime)
		
		println(ivals.mkString("\n"))

		val dates = ivals.map(ival =>
		  interpDate(ival.recordedAt, ival.time, ival.start, ival.end, stationDist).getTime)
		
		println("* Identified " + ivals.size + " intervals")
		
		ctxt.complete(jsonp(callback,write(dates)));
	      })
	  }
	}
      } ~
      pathPrefix("to" / "\\d+".r / "[^/]+".r) { (endStation, direction) =>
        path("") {
          parameters('callback ?, 'seriesTimeIncrement ? "0", 'numberOfRuns ? "0") {
            (callback, seriesTimeIncrement, numberOfRuns) => 
              parameters('segmentSizeKm ? "0.1", 
                         'startDate ? (new Date().getTime() - 1000*60*60*24).toString,
                         'endDate ? new Date().getTime().toString,
                         'startTime ? (curTime() - 2.0).toString,
                         'endTime ? curTime().toString) {
                (segmentSizeKm, startDate, endDate, startTime, endTime) =>
                  get(ctxt => {
                    val d = DirectionFactory.directionForString(direction).get

                    val station1 = loader.loadStation(stationId.toInt)                    
                    val station2 = loader.loadStation(endStation.toInt)

                    val possibleRoutes1 = loader.routesForStation(stationId.toInt)
                    val possibleRoutes2 = loader.routesForStation(endStation.toInt)

                    println("* Station 1 routes:")
                    println("\t" + possibleRoutes1)
                    println("* Station 2 routes:")
                    println("\t" + possibleRoutes2)

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
                                        (parseTime(startTime.toDouble),
                                         parseTime(endTime.toDouble)),
                                        null)                

                      val models = if (numberOfRuns.toInt > 0)
                        expandModel(model,
                                    startTime.toDouble,
                                    endTime.toDouble - startTime.toDouble,
                                    seriesTimeIncrement.toDouble,
                                    numberOfRuns.toInt)
                                   else
                                     List(model)

                      println("* About to estimate the following models:")
                      println(models)

                      val routePoints = loader.loadRoutePoints(
                        Map("route_id" -> tgtRoute.id.toString))

                      val intervals = loader.loadIntervals(tgtRoute.id)

                      println("* Loaded " + intervals.size + " intervals")

                      val est = estimator.estimateNextBus(
                        LatLon(station2.head.lat.toDouble, 
                               station2.head.lon.toDouble),
                        routePoints,
                        List(BusRecord(
                          station1.head.lat,
                          station1.head.lon,
                          "", "", "", "", "", "0")),
                        models,
                        intervals)
                      
                      println("Estimate: " + est)

                      est match {
                        case Some(List(BusEst(_,_,_,_,_,_,arrv))) =>
                          ctxt.complete(jsonp(callback, write(arrv)))
                        case _ => ctxt.fail(HttpStatusCodes.InternalServerError)
                      }
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
                         'startTime ? (curTime() - 2.0).toString,
                         'endTime ? curTime().toString) {
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

                          println("\t* Loaded " + routePoints.size + " route points")
                          println("\t* Loaded " + intervals.size + " intervals")
                          val model = Model(route.id, segmentSizeKm.toDouble, 10,
                                            (new Date(startDate.toLong),
                                             Some(new Date(endDate.toLong))),
                                            (parseTime(startTime.toDouble),
                                             parseTime(endTime.toDouble)),
                                            null) 
                          
                          estimator.estimateNextBus(stationLatLon,
                                                    routePoints,
                                                    busses,
                                                    List(model),
                                                    intervals)
                        })
                          
			val busArray = busEsts.flatten.flatten
			val trimmedBusArray = busArray.foldLeft(Map[String,BusEst]()) { (map,est) => map + ((est.busId, est)) }.values.toList

                        println("* Results: " + busArray)
			println("\t Trimmed: " + trimmedBusArray)

                        ctxt.complete(
                          jsonp(callback, write(trimmedBusArray)))
                        
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
