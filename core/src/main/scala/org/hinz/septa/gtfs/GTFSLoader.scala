package org.hinz.septa.gtfs

import java.io._
import scala.annotation.tailrec

object GTFSLoader {
  
  case class Station(id: String, name: String, lat: String, lon: String)

  private val rsrc:BufferedReader = new BufferedReader(new InputStreamReader(getClass().getResourceAsStream("/org/hinz/septa/gtfs/stops.txt")))
  rsrc.readLine() // Dump header row... we know what's up
  
  @tailrec
  private def readRouteData(reader: BufferedReader, acc: Map[String,Station] = Map()):Map[String,Station] = reader.readLine match {
    case null => acc
    case line => line.split(",").toList match {
      case List(id, name, lat, lon) => readRouteData(reader, acc + (id -> Station(id,name,lat,lon)))
      case _ => throw new Exception("Error parsing file... this really shouldn't happen though since we are providing the file as a resource")
    }
  }

  val stations = readRouteData(rsrc)

}

