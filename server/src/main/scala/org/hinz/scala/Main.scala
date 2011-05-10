package org.hinz.septa

import org.hinz.septa.server._
import scala.annotation.tailrec

object Main {
  val db = "/Users/ahinz/src/hobby/workhorse/devdb.db"
    
  val r = new RouteLoader(db)
  val lat = 40.008144
  val lon = -75.150421

  val routepts = r.loadRoutePoints(Map("route_id" -> "1"))
  println("Loaded " + routepts.length + " points!")

  val pts = RouteProcessor.distanceOnRoute(routepts, LatLon(lat,lon)).get

  println("pts... " + pts)

  def time =
    System.currentTimeMillis()

  @tailrec
  def executeEvery(msec: Int, f: Unit => Unit):Unit = {
    val cur = time
    f()
    Thread.sleep(msec - (time - cur))
    executeEvery(msec,f)
  }

  val server = new Server(new RouteLoader(db))

  def main(args: Array[String]) = {
    executeEvery(1000*60, Unit => server.runMe)
  }

}
