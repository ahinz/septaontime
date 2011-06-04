package org.hinz.septa.extractor

import org.hinz.septa._

/**
 * The Extractor attempts to insert route hints into the database
 */
class Extractor(db: String) {
  val loader:RouteLoader = new RouteLoader(db)
  
  /**
   * Identify the list of possible blocks on a given route
   */
  def blocks(route: String):List[String] = 
    loader.loadBusData(Map("route" -> route)).map(_.block).distinct.sortWith(_ > _)

  /**
   * Try to figure out when busses driving a given block switch routes
   */
  /*
   * Unfortunatly bus data only contains the route # (i.e. 44)
   * We actually need to go down and select the route interval
   * which contains the route_id which maps to a specific route
   * (i.e. 44 West via Ardmore)
   */
  def block(block: String) =  {
    // We really need to select all intervals and filter
    //TODO: This should be a raw sql query
    val sortedIntervals:List[Interval] =
      loader.loadBusData(Map("blocknum" -> block)).flatMap { busdata =>
        loader.loadIntervalsWhere(
          "route_data_id1=" + busdata.id + " or route_data_id2=" + busdata.id)}.
        sortWith(_.time < _.time)

    foldOverlaps(sortedIntervals).map(_.map(ival => ival.route_id).distinct)
  }

  // Fold overlapping intervals
  def foldOverlaps(ivals: List[Interval]) =
    ivals.map(ival => ivals.filter(ival.overlaps(_)))
}


object Main {
  val e = new Extractor("devdb.db")

  // Arguments:
  // -route X     Route # to lookup
  // -block K     Block # to lookup
  // -db 
  // You must specify one (and only one) of -route or -block
  def main(args: Array[String]) = args.toList match {
    case List("-route", route) => println("Blocks:\n\t" + e.blocks(route).mkString("\n\t"))
    case List("-block", block) => println(e.block(block).mkString("\n\t"))
    case List("-db", "read", block) => println("Not yet suppported!")
    case List("-db", "write", block) => println("Not yet supported!")
    case _ => help
  }

  def help =
    println("Bus block helper tool\n" +
            "Specify one (and only one) of the following arguments:\n" +
            "-route X           Display blocks that run on route X\n" +
            "-block X           Display info about the given block # (from bus data)\n" +
            "-db [read|write] X Read/write info about block X to the database\n")

}
  
