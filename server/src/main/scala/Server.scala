package org.hinz.septa.server

import java.util.Date
import java.util.Calendar

import scala.annotation.tailrec
import scala.io._

import net.liftweb.json.JsonParser._

import org.hinz.septa._

case class BusRecord(lat:String,lng:String,label:String,VehicleID:String,BlockID:String,Direction:String,destination:String,Offset:String)
case class BusRecords(bus: List[BusRecord])


class Server(ld:RouteLoader) {

  implicit val formats = net.liftweb.json.DefaultFormats

  val liveRoute = Source.fromFile("route.url").getLines.mkString("")

  def getMostRecentLiveData(route: String) =
    parse(Source.fromURL(liveRoute  + route).getLines.mkString("")).extract[BusRecords]


  def pollMostRecentBusData(busid: String, blockid: String) =
    ld.loadBusData(Map("blocknum" -> blockid,"busnum" -> busid),"recorded_at",1)

  /**
   * Check to see if the live data is newer than the db
   */
  def isNewData(db:BusData, live:BusRecord) = {
    val cal = Calendar.getInstance()
    cal.setTime(new Date())
    cal.set(Calendar.SECOND, 0)
    cal.set(Calendar.MINUTE, cal.get(Calendar.MINUTE) - live.Offset.toInt)
    val dateOfLiveUpdate = cal.getTime()
    val dateOfDBData = db.time

    dateOfLiveUpdate.compareTo(dateOfDBData) > 0
  }

  def isTooLongAgo(d1:Date, d2:Date) = 
    math.abs(d1.getTime - d2.getTime) > 18000000


  def routes(dir:Direction) = 
    ld.loadRoutes(Map("direction" -> dir.db))

  def matchingRoutes(bd1:BusData, bd2:BusData, routes:List[Route]):List[Interval] = {
    routes.map(r => {
      println(" --> Trying route " + r.shortname)
      val routeData = ld.loadRoutePoints(Map("route_id" -> r.id.toString))

      val rslt = RouteProcessor.distanceOnRoute(routeData,LatLon(bd1.lat,bd1.lon))

      if (rslt.isDefined) {
        val rslt2 = RouteProcessor.distanceOnRoute(routeData,LatLon(bd2.lat,bd2.lon))

        if (rslt2.isDefined) {
          println("      Positive Match!")
          Some(Interval(
            -1,r.id,bd1.id,bd2.id,rslt.get,rslt2.get, new Date(), 
            math.abs(bd1.time.getTime - bd2.time.getTime) / 1000.0))
        } else {
          println("     (Failed 2nd chance)")
          None
        }
      } else {
        println("     (Failed 1st chance)")
        None
      }}).flatten
  }

  def runMe = {
    val route = "23"
    val liveData = getMostRecentLiveData(route).bus.filter(_.Direction == "NorthBound")
    
    liveData.map((a:BusRecord) => {
      val db = pollMostRecentBusData(a.VehicleID,a.BlockID)
      val newRecord = BusData(-1,route,a.lat.toDouble,a.lng.toDouble, new Date(),a.BlockID,a.VehicleID)
      
      if (db.length == 0 || isTooLongAgo(db.head.time, new Date())) {
        println("No previous data available for this bus/block")
        println("Creating new bus data...")
        ld.createBusData(newRecord)
      } else if (isNewData(db.head,a)) {
        println("Creating an interval between " +
                db.head + " and " + newRecord)
        val inserted = ld.createBusData(newRecord)
        val prev = db.head
        
        val newIntervals = matchingRoutes(prev,inserted, routes(North))
        newIntervals.map(ld.createInterval(_))
      }
      else
        println("No update... stale real-time data")
    })

    println(" -- End of Cycle --\n\n\n")
  }
}

