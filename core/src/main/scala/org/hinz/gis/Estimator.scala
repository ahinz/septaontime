package org.hinz.gis

import org.hinz.septa.{Interval => DInterval,_}
import java.util.Date

import scala.annotation.tailrec

case class BusEst(blockId: String, busId: String, station: LatLon, origOffset: Double, offset: Double, arrival: Date) {
  def arrival(v: Date):BusEst = BusEst(blockId, busId, station, origOffset, offset, v)
}


/**
 * Used to estimate time between two distance points based
 * on a set of measured intervals
 *
 * @param combiner a function that takes a list of intervals
 * that all represent the same interval but with different time data
 * and computes an estimate time for that interval
 */
class Estimator { 

  /**
   *
   * @param station Lat/Lon for the station to check
   * @param route points on the given route
   * @param buses live bus points
   * @param ivals Possible interval matches
   *
   * @return List of bus estimates
   */
  def estimateNextBus(station: LatLon, route:List[RoutePoint], buses: List[BusRecord], ivals:List[Interval]):List[BusEst] = {
    
    // Determine linear ref for the station
    val sref = RouteProcessor.distanceOnRoute(route, station).get

    // Convert each bus to a linear ref and discard busses
    // that have already arrived at the destination
    val brefs:List[BusEst] = buses.map(x =>
      BusEst(x.BlockID, 
             x.VehicleID, 
             station, 
             x.Offset.toDouble, 
             RouteProcessor.distanceOnRoute(
               route, 
               LatLon(x.lat.toDouble,x.lng.toDouble)).getOrElse(-1.0), null)).filter(
                 x => x.offset >= 0 && x.offset < sref)
    
    val t = new Date().getTime
    
    // Convert from time offset (in seconds) to a data
    // also substract original delay (in minutes)
    brefs.map(x => 
      x.arrival(
        new Date(t - 
                 (x.origOffset * 60.0 * 1000.0).toLong + 
                 (estimate(x.offset, sref, ivals) * 1000).toLong))).sortWith(
                   _.arrival.getTime < _.arrival.getTime)
  }

  var log = false
  var segSize = 0.1 // 100 meters
 
  /**
   * Given a start and an end point attempt to guess how long it will
   * take to traverse the distance based on sample intervals
   *
   * The algorithm split the start/end distance into segments of size
   * segSize and then finds an average velocity based on some combination of
   * the overlapping intervals
   *
   * @param startDist start linear ref
   * @param endDist ending linear ref
   * @param intervals sample intervals
   * @param est time offset estimate
   * @return best estimate for time from start to end
   */
  @tailrec
  final def estimate(startDist:Double, endDist:Double, intervals:List[Interval], est:Double = 0):Double = {
    if (startDist >= endDist) est
    else {
      val thisInterval = Interval(startDist,startDist+segSize,0,null)
      printlg("About to process " + thisInterval + "... ")

      val inRange = intervals.filter(x => x.start <= thisInterval.end && x.end >= thisInterval.start)

      printlg(" #I: " + inRange.length)

      // Compute an average speed:
      // Only use the 4 most recent samples...
      val takeSize = 4
      val dataPoints = inRange.map(_.velocity).take(takeSize)
      val spd = dataPoints.reduceLeft(_ + _) / dataPoints.size
      val combd = segSize / spd

      printlg(" Avg V: " + spd)
      printlnlg(" Est: " + combd)
      estimate(thisInterval.end, endDist, intervals, est + combd)
    }
  }
  
  def printlg(x:String) = if (log) print(x)
  def printlnlg(x:String) = if (log) println(x)

}
