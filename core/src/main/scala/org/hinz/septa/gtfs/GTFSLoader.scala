package org.hinz.septa.gtfs

import org.hinz.septa._

import java.io._
import scala.annotation.tailrec

object GTFSLoader {

  private val rsrc:BufferedReader = new BufferedReader(new InputStreamReader(getClass().getResourceAsStream("/org/hinz/septa/gtfs/stops.txt")))
  rsrc.readLine() // Dump header row... we know what's up
  
  private def trips:BufferedReader = {
    val rdr = new BufferedReader(new InputStreamReader(new FileInputStream("/Users/ahinz/Downloads//Archive 00/septa_gtfs/google_bus/trips.txt")))
    rdr.readLine
    rdr
  }

  private def stopTimes:BufferedReader = {
    val rdr = new BufferedReader(new InputStreamReader(new FileInputStream("/Users/ahinz/Downloads//Archive 00/septa_gtfs/google_bus/stop_times.txt")))
    rdr.readLine
    rdr
  }

  private def routes:BufferedReader = {
    val rdr = new BufferedReader(new InputStreamReader(new FileInputStream("/Users/ahinz/Downloads//Archive 00/septa_gtfs/google_bus/routes.txt")))
    rdr.readLine
    rdr
  }

  @tailrec
  final def routeIDMap(reader: BufferedReader = routes, acc: Map[String,String] = Map()):Map[String,String] = reader.readLine match {
    case null => acc
    case line => line.split(",").toList match {
      case route_id::route_short_name::r =>
        routeIDMap(reader, acc + (route_id -> route_short_name))
      case _ => throw new Exception("error parsing file / " + line.split(","))
    }
  }

  @tailrec
  final def tripIDMap(reader: BufferedReader = trips, acc: Map[String,(String,String)] = Map()): Map[String,(String,String)] = reader.readLine match {
    case null => acc
    case line => line.split(",").toList match {
      case List(route_id,service_id,trip_id,block_id,direction_id,shape_id) =>
        tripIDMap(reader, acc + (trip_id -> (route_id, direction_id)))
      case _ => throw new Exception("error parsing file")
    }
  }

  @tailrec
  final def stationIDMap(reader: BufferedReader = stopTimes, acc: Map[String, List[String]] = Map()): Map[String, List[String]] = reader.readLine match {
    case null => acc
    case line => line.split(",").toList match {
      case List(trip_id,arrival_time,departure_time,stop_id,stop_sequence) =>
        stationIDMap(reader, getAndAppend(acc, stop_id, trip_id))
      case _ => throw new Exception("error parsing file")
    }
  }
      
  def createStationMap():Map[String,List[(String,String)]] = {
    println("* Starting to read route id map...")
    val routeMap = routeIDMap() // Route Name => Id [44 -> 2052]
    println("* Starting to read trip map...")
    val tripMap = tripIDMap() // Trip ID -> (RouteID,DirectionID) [142 -> (2052,0)]
    println("* Starting to read station id map...")
    val stationMap = stationIDMap() // stop_id -> List[TripID]
    println("* Starting to compile final map...")
    
    stationMap.map(stopIdAndTripIds => {
      val stopId = stopIdAndTripIds._1
      val tripIds = stopIdAndTripIds._2
      val routesAndDirections = tripIds.map(tripMap.get(_).get)
      
      val routesAndDirs:List[(String,String)] = routesAndDirections.map(routeAndDir => {
        val route = routeAndDir._1
        val dir = routeAndDir._2

        (routeMap.get(route).get, dir)
      }).distinct

      (stopId, routesAndDirs)
    })
  }

  def getAndAppend(m: Map[String,List[String]], key: String, value: String) = {
    val newValue = m.get(key) match {
      case Some(vals) => value :: vals
      case None => List(value)
    }

    val newPair:(String,List[String]) = (key, newValue)
    m + newPair
    }    

  @tailrec
  private def readRouteData(reader: BufferedReader, acc: Map[String,Station] = Map()):Map[String,Station] = reader.readLine match {
    case null => acc
    case line => line.split(",").toList match {
      case List(id, name, lat, lon) => readRouteData(reader, acc + (id -> Station(id,name,lat,lon)))
      case _ => throw new Exception("Error parsing file... this really shouldn't happen though since we are providing the file as a resource")
    }
  }

  lazy val stations = readRouteData(rsrc)

}

