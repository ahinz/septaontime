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
 * Mapping services
 *
 * Routes:
 * /map/{route}/{lat1},{lon1}/to/{lat2},{lon2}/
 *     Return lat/lon points for the given route. There is no est data returned.
 * 
 * /map/intervals/{route}/{dir}/{lat},{lon}/
 *     Return time est for each summary interval
 * Params:
 *  map    boolean       sepecify 0 to exclude any gis data from being returned [default 1]
 * 
 */
trait MappingServiceBuilder extends ServiceBuilder {
  implicit val mappingFormats = Serialization.formats(NoTypeHints)

  val fixedDataLoader:FixedDataLoader
  val loader: RouteLoader
  val estimator: Estimator

  val mapService = {
    pathPrefix("map" / "intervals" / "[^/]*".r / "[^/]+".r / "([^,]+),".r ~ "[^/]+".r) {
      (routeNum, dir, lat, lon) =>
	path("") {
	  parameters('callback ?, 'map ?) {
	    (callback, map) =>
	      get(ctxt => {
		val pt = LatLon(lat.toDouble, lon.toDouble)

                val possibleRoutes = loader.loadRoutes(Map("shortname" -> routeNum))
                
                println("* Identified the following possible routes: \n" + possibleRoutes)
                
                val routePts = possibleRoutes.map(r => 
                  (r.id, loader.loadRoutePoints(Map("route_id" -> r.id.toString))))

                val ptPos = routePts.map(rte => 
		  if(estimator.nearestPointOnRoute(rte._2,pt).isDefined)
		    Some(rte._1)
		  else
		    None).flatten

		if (ptPos == Nil) {
		  println("* Error the given point was not on the route!")
		  ctxt.fail(HttpStatusCodes.NotFound)
		} else {
		  val today = new Date()
		  val yesterday = new Date(today.getTime() - 1000*60*60*24 + 1)
		
		  val routeId = ptPos.head
  
		  val cal = Calendar.getInstance()
		  
		  cal.set(Calendar.HOUR_OF_DAY, 0)
		  cal.set(Calendar.MINUTE, 0)
		  
		  val midnight_am = cal.getTime()
		  
		  cal.set(Calendar.HOUR_OF_DAY, 23)
		  cal.set(Calendar.MINUTE, 59)
		  val midnight_pm = cal.getTime()
		  
		  val ivals = loader.loadIntervals(routeId)
		  val model = Model(routeId, 
				    0.1, 
				    10,
				    (yesterday, Some(today)),  
				    (midnight_am, midnight_pm),
				    (0, ivals.map(_.end).max(Ordering[Double])))
		  
		  val ests = estimator.estimateIntervals(List(model), ivals)
		  
		  if (map == "0") {
		    ctxt.complete(jsonp(callback, write(ests)))
		  } else {
		    ctxt.complete(jsonp(callback, write(fillOut(routeId, ests))))
		  }
		}
	      })
	  }
	}
    } ~
    pathPrefix("map" / "\\d+".r / "([^,]+),".r ~ "[^/]+".r / "to" / "([^,]+),".r ~ "[^/]+".r) {
      (routeId, lat1, lon1, lat2, lon2) =>
        path("")(
          parameter('callback ?) { 
            callback =>
              get(ctxt => { 
                // Attempt to resolve locations...
                println("* Trying to get points for route " + routeId)
                val possibleRoutes = loader.loadRoutes(Map("shortname" -> routeId))
                
                val loc1 = LatLon(lat1.toDouble, lon1.toDouble)
                val loc2 = LatLon(lat2.toDouble, lon2.toDouble)
                
                println("* Identified the following possible routes: \n" + possibleRoutes)
                
                val routePts = possibleRoutes.map(r => 
                  loader.loadRoutePoints(Map("route_id" -> r.id.toString)))
                
                val loc1pos = routePts.map(estimator.nearestPointOnRoute(_,loc1))
                val loc2pos = routePts.map(estimator.nearestPointOnRoute(_,loc2))
                
                val matches = possibleRoutes.zip(loc1pos.zip(loc2pos)).filter(tpl => tpl._2._1.isDefined && tpl._2._2.isDefined)
                
                if (matches.size == 0) {
                  ctxt.complete(jsonp(callback, write("error - lat/lon not on same route")))
                } else {
                  val routeAndDists = matches.head
                  val dbRouteId = routeAndDists._1.id.toInt
                  val loc1dist = routeAndDists._2._1.get.distanceTo(loc1)
                  val loc2dist = routeAndDists._2._2.get.distanceTo(loc2)

                  val start = loc1dist.min(loc2dist)
                  val end = loc1dist.max(loc2dist) 

                  val pts = fillOut(dbRouteId, List(EstInterval(start,end, List(),List())))

                  ctxt.complete(jsonp(callback,write(pts.head.pts)))
                }
              })
          })
    }
  }

  def interpll(dist: Double, routeId: Int) = {
    val start_list = loader.loadWithBuilder("select * from route_data where ref > " + dist + " and route_id = " + routeId + " limit 1", loader.buildRoutePoint _)
    val stop_list = loader.loadWithBuilder("select * from route_data where ref < " + dist + " and route_id = " + routeId + " order by ref desc limit 1", loader.buildRoutePoint _)
    
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
    PEstInterval(List(List(interpll(ival.startDist, routeId)), 
                      loadPointsOnRoute(ival.startDist, ival.endDist, routeId), List(interpll(ival.endDist, routeId))).flatten, ival)
  }
  
  def fillOut(routeId: Int, ivals: List[EstInterval]):List[PEstInterval] = ivals.map(fillOut(routeId, _))

  def loadPointsOnRoute(start: Double, stop: Double, routeId: Int):List[LatLon] =
    loader.loadWithBuilder("select * from route_data where ref > " + start + " and ref < " + stop + " and route_id = " + routeId, loader.buildRoutePoint _).map(x => LatLon(x.lat,x.lon))

}
