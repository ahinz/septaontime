package org.hinz.septa

import java.sql.{Array => SqlArray,Date => SqlDate,_}
import java.util.Date
import java.text._


/**
 * Route Loader is responsible for loading routes and other objects
 * from the database
 */
class RouteLoader(db: String) {

  Class.forName("org.sqlite.JDBC")

  def runStatement[T](s: Statement => T):Option[T] = {
    val c = DriverManager.getConnection("jdbc:sqlite:" + db)
    var t:Option[T] = None
    try {
      t = Some(s(c.createStatement))
    } finally {
      c.close()
    }
    t
  }
  
  def exhaustResultSet[T](r: ResultSet, f:(ResultSet => T), acc:List[T]=Nil):List[T] =
    if (r.next())
      exhaustResultSet(r, f, f(r) :: acc)
    else
      acc.reverse

  def decodeDirection(d: String):Direction =
    if (d == "n")
      North
    else if (d == "w")
      West
    else if (d == "s")
      South
    else
      East

  private def buildRoute(rs: ResultSet) =
    Route(rs.getInt("id"), rs.getString("shortname"), rs.getString("longname"), rs.getString("desc"), decodeDirection(rs.getString("direction")))

  // Sun May 08 17:13:07 EDT 2011
  val format = new SimpleDateFormat("EEE MMM dd HH:mm:ss z yyyy")

  private def buildInterval(rs: ResultSet) =
    Interval(rs.getInt("id"), rs.getInt("route_id"), rs.getInt("route_data_id1"),rs.getInt("route_data_id2"),rs.getDouble("start_ref"),rs.getDouble("end_ref"),format.parse(rs.getString("recordedAt")),rs.getDouble("time"))

  private def buildBusData(rs: ResultSet) =
    BusData(rs.getInt("id"), rs.getString("route"), rs.getDouble("lat"),
            rs.getDouble("lon"),format.parse(rs.getString("recorded_at")), 
            rs.getString("blocknum"),rs.getString("busnum"))
  

  private def buildRoutePoint(rs: ResultSet) =
    RoutePoint(rs.getInt("id"), rs.getInt("route_id"), rs.getDouble("lat"),
               rs.getDouble("lon"), rs.getDouble("ref"))

  def buildWhereClause(where: Map[String,String]) =
    if (where == null) ""
    else " where " + where.map(p => p._1 + "='" + p._2 + "'").mkString(" AND ")

  def buildLimitClause(limit: Int) =
    if (limit == -1) ""
    else " LIMIT " + limit

  def buildOrderByClause(order: String) =
    if (order == null) " "
    else " ORDER BY " + order + " DESC"

  def loadWithBuilder[T](sel: String, f: ResultSet => T) = { //println(sel); 
    runStatement(stmt =>
      exhaustResultSet(stmt.executeQuery(sel), f)) getOrElse List()}

  def loadRoutes(where: Map[String,String] = null):List[Route] = 
    loadWithBuilder("select * from route" + buildWhereClause(where), buildRoute _)

  def loadBusData(where: Map[String,String] = null, orderby:String = null, limit:Int = -1):List[BusData] =
    loadWithBuilder("select * from bus_data" + buildWhereClause(where) + buildOrderByClause(orderby) + buildLimitClause(limit), buildBusData _)

  def loadRoutePoints(where: Map[String,String] = null):List[RoutePoint] =
    loadWithBuilder("select * from route_data " + buildWhereClause(where), buildRoutePoint _)

  def loadIntervalsWhere(where: String) =
    loadWithBuilder("select * from interval_data where " + where, buildInterval _)

  def loadIntervals(route: Int) = {
    val stmt = "select * from interval_data where route_id=" + route
    loadWithBuilder(stmt, buildInterval _)
  }

  def loadIntervals(route: Int, startRef:Double, endRef:Double) = {
    val stmt = "select * from interval_data where route_id=" + route + " and start_ref <= " + endRef + " AND end_ref >= " + startRef
    println(stmt)
    loadWithBuilder(stmt, buildInterval _)
  }

  def createInterval(t: Interval) = {
    val stmtstr = "insert into interval_data (route_id,route_data_id1,route_data_id2,start_ref,end_ref,recordedAt,time) values (" + List(t.route_id, t.bus_data_id1, t.bus_data_id2,t.start,t.end,t.recordedAt,t.time).map("'" + _ + "'").mkString(",") + ")"

    println("STMT: " + stmtstr)
    runStatement(stmt =>
      stmt.executeUpdate(stmtstr))

  }

  def createRouteData(d: RoutePoint) = {
    val stmtstr = "insert into route_data (route_id,lat,lon,ref) values (" + d.route_id + "," + d.lat + "," + d.lon + "," + d.ref + ")"
    runStatement(stmt =>
      stmt.executeUpdate(stmtstr))
  }

  def createRoute(r: Route) = {
    val stmtstr = "insert into route (shortname, longname, desc, direction) values (" +
    List(r.shortname,r.longname,r.desc,r.direction.db).map("\"" + _ + "\"").mkString(",") + ")"

    runStatement(stmt =>
      stmt.executeUpdate(stmtstr))
    loadRoutes(Map("shortname" -> r.shortname, "longname" -> r.longname, "desc" -> r.desc, "direction" -> r.direction.db)).head
  }

  def createBusData(d: BusData) = {
    val stmtstr = "insert into bus_data(route,lat,lon,recorded_at,blocknum,busnum) values (" + List(d.route,d.lat,d.lon,d.time,d.block,d.bus).map("'" + _ + "'").mkString(",") + ")"
    //println("STMT: " + stmtstr)
    runStatement(stmt =>
      stmt.executeUpdate(stmtstr))
    loadBusData(Map("blocknum" -> d.block, "busnum" -> d.bus),"recorded_at",1).head
  }
    
}
