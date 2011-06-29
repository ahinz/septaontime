package org.hinz.septa

import org.hinz.gis._
import scala.io._
import net.liftweb.json.JsonParser._

case class BusRecord(lat:String,lng:String,label:String,VehicleID:String,BlockID:String,Direction:String,destination:String,Offset:String) {
  def toLatLon() = LatLon(lat.toDouble, lng.toDouble)
}

case class BusRecords(bus: List[BusRecord])


object LiveDataLoader {
  implicit val formats = net.liftweb.json.DefaultFormats

  val liveRoute = Source.fromFile("route.url").getLines.mkString("")

  def getMostRecentLiveData(route: String):List[BusRecord] =
    parse(Source.fromURL(liveRoute  + route).getLines.mkString("")).extract[BusRecords].bus

  
}
