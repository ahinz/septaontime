package org.hinz.gis

import org.hinz.septa._
import java.util.{Date,Calendar}
import java.awt.geom.Point2D

import scala.annotation.tailrec

case class LatLon(lat:Double,lon:Double) {
  def toPoint2D = new Point2D.Double(lon,lat)
}

/**
 * Parameters to the estimation model
 *
 * @param route The db id of the route to estimate
 * @param summarySegmentSizeKm The size of segment to summarize
 * @param maxNumberOfIntervals The maximum number of intervals to use for a given segment
 * @param startDate Start date of intervals
 * @param endDate Last date to use (specify None to use most up-to-date value)
 * @param upperTimeBound earliest hour/minute to use
 * @Param lowerTimeBound latest hour/minute to use
 */
case class Model(route: Int, summarySegmentSizeKm: Double, maxNumberOfIntervals: Int, startDate: Date, endDate: Option[Date], lowerTimeBound: Date, upperTimeBound: Date) {

  var ivalCache:Option[List[Interval]] = None

  //@todo: Test Me
  def createOutputIntervals(endDist: Double):List[(Double,Double)] = {
    val pts = (0 to (endDist / summarySegmentSizeKm).toInt).toList.map(_.toDouble * summarySegmentSizeKm) ++ List(endDist)

    pts.zip(pts.tail)
  }

  def maxDist(ld: RouteLoader): Double =
    loadIntervalsForModel(ld).map(_.end).max(Ordering[Double])

  //@todo: Test Me
  def loadIntervalsForModel(ld: RouteLoader): List[Interval] = ivalCache match {
    case Some(cache) => cache
    case None => {
      val intervals = ld.loadIntervals(route).filter(i => isInTimeRange(i.recordedAt, lowerTimeBound, upperTimeBound))
      
      var finalIntervals = endDate match {
	case Some(date) => intervals.filter(i => isInDateRange(i.recordedAt, startDate, date))
	case None => intervals
      }
      
      ivalCache = Some(finalIntervals.sortWith { (a,b) =>
	a.recordedAt.compareTo(b.recordedAt) > 0 })

      ivalCache.get
    }
  }

  //@todo: Test Me
  def isInDateRange(d: Date, startDate: Date, endDate:Date):Boolean =
    d.getTime() <= endDate.getTime() && d.getTime() >= startDate.getTime()

  //@todo: Test Me
  def isInTimeRange(date: Date, startTime: Date, endTime:Date):Boolean = {
    var startTimeCal = Calendar.getInstance()
    startTimeCal.setTime(startTime)
    var endTimeCal = Calendar.getInstance()
    endTimeCal.setTime(endTime)
    var dCal = Calendar.getInstance()
    dCal.setTime(date)

    var start = startTimeCal.get(Calendar.HOUR_OF_DAY) + startTimeCal.get(Calendar.MINUTE) / 60.0
    var end = endTimeCal.get(Calendar.HOUR_OF_DAY) + endTimeCal.get(Calendar.MINUTE) / 60.0
    var d = dCal.get(Calendar.HOUR_OF_DAY) + dCal.get(Calendar.MINUTE) / 60.0

    d <= end && d >= start
  }
}

case class BusEst(blockId: String, busId: String, station: LatLon, origOffset: Double, offset: Double, arrival: Date) {
  def arrival(v: Date):BusEst = BusEst(blockId, busId, station, origOffset, offset, v)
}

case class EstInterval(startDist: Double, endDist: Double, v: List[Double], t: List[Double])

/**
 * Used to estimate time between two distance points based
 * on a set of measured intervals
 *
 * @param combiner a function that takes a list of intervals
 * that all represent the same interval but with different time data
 * and computes an estimate time for that interval
 */
class Estimator { 

  //@todo test me!
  //@todo weighted average
  def getSpeedAndTime(takeSize: Int, dist: Double, ivals: List[Interval]) = {
    val dataPoints = ivals.map(_.velocity).take(takeSize)
    val spd = dataPoints.reduceLeft(_ + _) / dataPoints.size
    val time = dist / spd
    (spd, time)
  }


  //@todo test me!
  def estimateInterval(interval: (Double, Double), allIntervals: List[List[Interval]], takeSize: Int):EstInterval = {

    val dist = interval._2 - interval._1

    //@todo Weighted avg
    val spdAndTimes = allIntervals.map(intervals => getSpeedAndTime(takeSize, dist, intervals.filter(x => x.start <= interval._2 && x.end >= interval._1)))

    EstInterval(interval._1, 
		interval._2, 
		spdAndTimes.map(_._1 * 1000.0),
		spdAndTimes.map(_._2))		
  }

  def estimateIntervals(m: Model, ld: RouteLoader):List[EstInterval] =
    estimateIntervals(List(m), ld)

  //@todo test me!
  def estimateIntervals(m: List[Model], ld: RouteLoader):List[EstInterval] =
    estimateIntervals(m.head.createOutputIntervals(m.head.maxDist(ld)),
		      m.map(_.loadIntervalsForModel(ld)),
		      m.head.maxNumberOfIntervals)



  def estimateIntervals(targetIntervals: List[(Double,Double)], measuredIntervals: List[List[Interval]], takeSize: Int) =
    targetIntervals.map(tgt => estimateInterval(tgt, measuredIntervals, takeSize))


  /**
   *
   * @param station Lat/Lon for the station to check
   * @param route points on the given route
   * @param buses live bus points
   * @param ivals Possible interval matches
   *
   * @return List of bus estimates
   */
  def estimateNextBus(station: LatLon, route:List[RoutePoint], buses: List[BusRecord], ivals:List[Interval]):Either[String,List[BusEst]] = {
    
    // Determine linear ref for the station
    val srefOpt = distanceOnRoute(route, station)

    if (srefOpt.isDefined) {
      val sref = srefOpt.get

      // Convert each bus to a linear ref and discard busses
      // that have already arrived at the destination
      val brefs:List[BusEst] = buses.map(x =>
        BusEst(x.BlockID, 
               x.VehicleID, 
               station, 
               x.Offset.toDouble, 
               distanceOnRoute(
                 route, 
                 LatLon(x.lat.toDouble,x.lng.toDouble)).getOrElse(-1.0), null)).filter(
                   x => x.offset >= 0 && x.offset < sref)
      
      val t = new Date().getTime
      
      // Convert from time offset (in seconds) to a data
      // also substract original delay (in minutes)
      Right(brefs.map(x => 
        x.arrival(
          new Date(t - 
                   (x.origOffset * 60.0 * 1000.0).toLong + 
                   (estimate(x.offset, sref, ivals) * 1000).toLong))).sortWith(
                     _.arrival.getTime < _.arrival.getTime))
    } else {
      Left("Error - station lat/lon not found on the given route")
    }
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
      val thisInterval = (startDist,startDist+segSize)
      printlg("About to process " + thisInterval + "... ")

      val inRange = intervals.filter(x => x.start <= thisInterval._2 && x.end >= thisInterval._1)

      printlg(" #I: " + inRange.length)

      // Compute an average speed:
      // Only use the 4 most recent samples...
      val takeSize = 4
      val dataPoints = inRange.map(_.velocity).take(takeSize)
      val spd = dataPoints.reduceLeft(_ + _) / dataPoints.size
      val combd = segSize / spd

      printlg(" Avg V: " + spd)
      printlnlg(" Est: " + combd)
      estimate(thisInterval._2, endDist, intervals, est + combd)
    }
  }
  
  def fudgeLat = 1.0/3600.0
  def fudgeLon = 1.0/3600.0

  /**
   * Find the smallest distance between the given route and
   * the given point
   *
   * If no distance less than a preset threshold can be found
   * this method returns None
   */
  def distanceOnRoute(route: List[RoutePoint], pt:LatLon):Option[Double] = {
    
    // Determine if any route point pair could contain this interval
    val minDist = 0.01

    val m =
      route.zip(route.tail).foldLeft((None:Option[(RoutePoint,RoutePoint)],minDist))((curmin,p) => {
        val p1 = p._1
        val p2 = p._2

        val maxlat = p1.lat.max(p2.lat)
        val minlat = p1.lat.min(p2.lat)
        val maxlon = p1.lon.max(p2.lon)
        val minlon = p1.lon.min(p2.lon)

        if (pt.lat >= minlat - fudgeLat  && pt.lat <= maxlat + fudgeLat &&
            pt.lon >= minlon - fudgeLon && pt.lon <= maxlon + fudgeLon) {

          val minDist = GIS.minDistance((pt.lon,pt.lat),
                                      GIS.computeLine(p1.lon,p1.lat,p2.lon,p2.lat))

          if (minDist < curmin._2)
            (Some((p1,p2)), minDist)
          else
            curmin
        } else {
          curmin
        }
      })
   
    m._1.map( p => p._1.ref + GIS.distanceCalculator(p._1.lat,p._1.lon,pt.lat,pt.lon) )
  }

  def printlg(x:String) = if (log) print(x)
  def printlnlg(x:String) = if (log) println(x)

}
