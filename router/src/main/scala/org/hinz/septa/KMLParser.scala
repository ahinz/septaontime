package org.hinz.septa

import org.hinz.gis._

import java.sql.Connection
import java.sql.DriverManager
import java.sql.ResultSet
import java.sql.SQLException
import java.sql.Statement

import scala.xml._
import scala.io.Source


class DBWriter(db:String) {
  Class.forName("org.sqlite.JDBC")

  def writeSinglePoint(statement:Statement, rid:Int,  lat1: Double, lon1: Double, dist: Double) = {
    val stmt = "insert into route_data (route_id, lat, lon, ref) values (" +
                            rid + ", " + lat1 + ", " + lon1 + ", " + dist + ")"
    println(stmt)
   // statement.executeUpdate(stmt)
  }

  def writePoints(routeid:Int, pts: List[(Double,Double)]) = {
    val connection = DriverManager.getConnection(
      "jdbc:sqlite:devdb.db")
    val statement = connection.createStatement();
    
    addDistanceToPts(pts).map(x => writeSinglePoint(statement, routeid, x._1,x._2,x._3))
  }

  def addDistanceToPts(l: List[(Double,Double)], acc: List[(Double,Double,Double)]=Nil):List[(Double,Double,Double)] = l match {
    case Nil => acc.reverse
    case x::xs => 
      if (acc.length == 0)
        addDistanceToPts(xs, (x._1,x._2,0.0) :: acc)
      else
        addDistanceToPts(xs, (x._1,x._2,acc.head._3 + 
                              GIS.distanceCalculator(x._1,x._2,acc.head._1,acc.head._2)) :: acc)
  }
        
}

