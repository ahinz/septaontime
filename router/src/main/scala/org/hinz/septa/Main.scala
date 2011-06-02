package org.hinz.septa

import org.hinz.septa.RouteCreator._
import org.hinz.septa.Routes._

import scala.swing._

object Main {
  def log(s: String) = print("* " + s)
  def logln(s: String) = log(s + "\n")

  def main(args: Array[String]) = {
 
    // Create default routes
    val db = "devdb.db"
    val ld = new RouteLoader(db)
    
    log("Starting to clean the database...")

    ld.runStatement(stmt => {
      stmt.executeUpdate("delete from route")
      stmt.executeUpdate("delete from route_data")
      stmt.executeUpdate("delete from interval_data")
      stmt.executeUpdate("delete from bus_data")
    })

    logln("done")


    Routes.route44.map(r => {
      logln("Trying to create route " + r)
      createRoute(r, ld)})                  
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

