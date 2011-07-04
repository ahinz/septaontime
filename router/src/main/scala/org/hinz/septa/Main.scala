package org.hinz.septa

import org.hinz.septa.RouteCreator._
import org.hinz.septa.Routes._
import org.hinz.septa.gtfs._

import scala.swing._

object Main {
  def log(s: String) = print("* " + s)
  def logln(s: String) = log(s + "\n")

  def main(args: Array[String]) = {
 
    // Create default routes
    val db = "devdb.db"
    val ld = new RouteLoader(db)
    
    log("Starting to clean the database...")

/*
    ld.runStatement(stmt => {
      stmt.executeUpdate("delete from route")
      stmt.executeUpdate("delete from route_data")
      stmt.executeUpdate("delete from interval_data")
      stmt.executeUpdate("delete from bus_data")
    })
*/
    logln("done")
/*
    try {
      ld.runStatement(stmt => {
        stmt.executeUpdate("drop table stations")
      })
    } catch {
      case e:Exception => println("--- stations table does not exist")
    }

    ld.runStatement(stmt => {
      stmt.executeUpdate("create table stations (id int, name string, lat string, lon string)")
      stmt.executeUpdate("create index stationId on stations (id)")
      GTFSLoader.stations.values map(s =>
        stmt.executeUpdate(
          "insert into stations values (" + s.id.toInt + ", \"" + 
          s.name + "\",\"" + s.lat + "\",\"" + s.lon + "\")"))
    })
*/

/*
    Routes.route44.map(r => {
      logln("Trying to create route " + r)
      createRoute(r, ld)})                  
*/

    try {
      ld.runStatement(stmt => {
        stmt.executeUpdate("drop table station_route")
      })
    } catch {
      case e:Exception => println("--- station route table does not exist [" + e + "]")
    }

    val stationMap:Map[String,List[(String,String)]] =
      GTFSLoader.createStationMap()

    println("--- Got that station map (" + stationMap.size + " records). About to create database records")

    ld.runStatement(stmt => {
      stmt.executeUpdate("create table station_route (station_id int, route_shortname string, direction string)")
      stmt.executeUpdate("create index station_route_index on station_route (station_id)")
    })

    println("--- Created tables")

    stationMap.map(stationIdAndRouteDir => {
      val stationId = stationIdAndRouteDir._1
      val routeDirs = stationIdAndRouteDir._2

      ld.runStatement(stmt => {
        routeDirs.map(routeDir =>  
          stmt.executeUpdate("insert into station_route values (" +
                             stationId + ", \"" + routeDir._1 + "\",\"" + 
                             routeDir._2 + "\")"))
      })
    })

/*
    val top = new MainFrame {
      size = new java.awt.Dimension(1000,600)
      preferredSize = new Dimension(1000,600)
      title = "Hello, World!"
      contents = new GISPanel(processRoute(route44_e_dt3, loadFromXML(route44_e_dt2.r)))
    }

    top.pack
    top.visible = true
*/

  }
}

