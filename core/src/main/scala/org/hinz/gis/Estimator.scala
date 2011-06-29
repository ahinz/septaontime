package org.hinz.gis

import org.hinz.septa._
import java.util.{Date,Calendar}
import java.awt.geom.Point2D

import scala.annotation.tailrec

case class LatLon(lat:Double,lon:Double) {
  def toPoint2D = new Point2D.Double(lon,lat)
}

/**
 * Convience object for building modesl
 */
object Model {
  def now = new Date().getTime
  val day = 1000 * 60 * 60 * 24

  def timeHoursAgo(hours: Int, minutes: Int = 0) = {
    val cal = Calendar.getInstance()
    cal.setTime(new Date())
    cal.set(Calendar.HOUR_OF_DAY, cal.get(Calendar.HOUR_OF_DAY) - hours)
    cal.set(Calendar.MINUTE, cal.get(Calendar.MINUTE) - minutes)

    (cal.getTime(), new Date())
  }

  def time24h = {
    val cal = Calendar.getInstance()

    cal.set(Calendar.HOUR_OF_DAY, 0)
    cal.set(Calendar.MINUTE, 0)
    
    val midnight_am = cal.getTime()

    cal.set(Calendar.HOUR_OF_DAY, 23)
    cal.set(Calendar.MINUTE, 59)
    val midnight_pm = cal.getTime()

    (midnight_am, midnight_pm)
  }

  def baseline(routeId:Int):List[Model] = List(
    Model(routeId,1.8,3,(new Date(now - day), None), timeHoursAgo(1), (0.0,0.0)))
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
 * @param lowerTimeBound latest hour/minute to use
 * @param startDist distance to start at
 * @param endDist distance to finish at or None for the entire route
 */
case class Model(route: Int, summarySegmentSizeKm: Double, maxNumberOfIntervals: Int, dateRange:(Date,Option[Date]), timeRange:(Date,Date), distRange:(Double,Double))
{
  def copyWithNewDistances(nstart: Double, nend:Double) =
    Model(route, summarySegmentSizeKm, maxNumberOfIntervals, dateRange, timeRange, (nstart, nend))

  def createOutputIntervals():List[(Double,Double)] = {
    if (distRange._1 < 0 || distRange._2 < 0 || distRange._2 < distRange._1)
      throw new IllegalArgumentException("range must not be negative and start must be > than finish")
    if (summarySegmentSizeKm <= 0)
      throw new IllegalArgumentException("summary size must be > 0")
    
    var pts = (0 to ((distRange._2 - distRange._1) / summarySegmentSizeKm).toInt).toList.map(distRange._1 + _.toDouble * summarySegmentSizeKm)

    if (pts.last != distRange._2) pts = pts ++ List(distRange._2)

    pts.zip(pts.tail)
  }

  def usesInterval(ival: Interval) =
    ival.recordedAt.getTime() >= dateRange._1.getTime() &&
    (!dateRange._2.isDefined || ival.recordedAt.getTime() < dateRange._2.get.getTime()) &&
    isInTimeRange(ival.recordedAt, timeRange._1, timeRange._2) &&
    ((ival.start >= distRange._1 && ival.start <= distRange._2) ||
     (ival.end >= distRange._1 && ival.end <= distRange._2) ||
     ival.end >= distRange._2 && ival.start <= distRange._1)

//@todo - handle "option" end date to make usesInterval more pretty
  def isInDateRange(d: Date, startDate: Date, endDate:Date):Boolean =
    d.getTime() <= endDate.getTime() && d.getTime() >= startDate.getTime()

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

case class BusEst(blockId: String, busId: String, station: LatLon, origOffset: Double, offset: Double, arrival: List[Date]) {
  def arrival(v: List[Date]):BusEst = BusEst(blockId, busId, station, origOffset, offset, v)
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

  def badSpeedData(d:Double):(Double, Double) = (0, d / 10.0)

  //@todo weighted average
  def getSpeedAndTime(dist: Double, ivals: List[Interval]):(Double,Double) = {
    if (ivals.size == 0 || dist < 0) // No Data... not sure what to do here?
      badSpeedData(dist) // just guess 10 mph.
    else {
      val spd = ivals.map(_.velocity).reduceLeft(_ + _) / ivals.size.toDouble
      println("spd @ ival: " + spd + " | " + ivals.size + " | " + ivals.take(3))

      if (spd == 0)
        badSpeedData(dist)
      else
        (spd, dist / spd)
    }
  }


  def estimateInterval(interval: (Double, Double), allIntervals: List[List[Interval]], models: List[Model]):EstInterval = {
    val dist = interval._2 - interval._1

    val prunedIntervals = allIntervals.zip(models).map(m =>
      m._1.filter(iv => m._2.usesInterval(iv) && iv.overlaps(interval._1, interval._2)))

    //@todo Weighted avg by date!
    val spdAndTimes = prunedIntervals.map(intervals => getSpeedAndTime(dist, intervals.filter(x => x.start <= interval._2 && x.end >= interval._1)))

    EstInterval(interval._1, 
		interval._2, 
		spdAndTimes.map(_._1 * 1000.0),
		spdAndTimes.map(_._2))		
  }

  def estimateIntervals(m: Model, intervals: List[Interval]):List[EstInterval] =
    estimateIntervals(List(m), intervals)

  def estimateIntervals(m: List[Model], intervals: List[Interval]):List[EstInterval] =
    if (m.size == 0) Nil
    else estimateIntervals(m.head.createOutputIntervals(),
                           m.map(x => intervals), // each model needs to have its own interval set. these intervals are pruned in estimateInterval
		           m) 

  def estimateIntervals(targetIntervals: List[(Double,Double)], measuredIntervals: List[List[Interval]], models: List[Model]) = {
    targetIntervals.map(tgt => estimateInterval(tgt, measuredIntervals, models))
  }

  def now:Long = new Date().getTime

  //@todo is kind of odd that each bus est contains many positions for each model
  def reduceIntervalsToDate(dataOffsetInMinutes: Int, evals: List[EstInterval]):List[Date] =
    reduceIntervalsToDate(dataOffsetInMinutes, evals.map(_.t))

  @tailrec
  final def reduceIntervalsToDate(dataOffsetInMinutes: Int, evals: List[List[Double]],times:List[Date] = Nil):List[Date] = 
    if (evals.size == 0 || evals.head.size == 0) times.reverse
    else {
      var split = evals.map(v => (v.head, v.tail))
      var cur = split.map(_._1)
      var rest = split.map(_._2)
      reduceIntervalsToDate(dataOffsetInMinutes, rest, reduceTimesToDate(dataOffsetInMinutes, cur) :: times)
    }

  def reduceTimesToDate(dataOffsetInMinutes: Int, times: List[Double]):Date =
    new Date((times.foldLeft(0.0)(_+_) * 1000.0).toLong + now - (dataOffsetInMinutes * 60.0 * 1000.0).toLong)

  /**
   *
   * @param station Lat/Lon for the station to check
   * @param route points on the given route
   * @param buses live bus points
   * @param ivals Possible interval matches
   *
   * @return List of bus estimates
   */
  //@todo test me!
  def estimateNextBus(station: LatLon, route:List[RoutePoint], buses: List[BusRecord], models: List[Model], ivals:List[Interval]):Either[String,List[BusEst]] = {
    
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
      
      // Convert from time offset (in seconds) to a data
      // also substract original delay (in minutes)
      Right(brefs.map(x => 
        x.arrival(
	  reduceIntervalsToDate(x.origOffset.toInt, estimateIntervals(models.map(_.copyWithNewDistances(x.offset, sref)), ivals)))).sortWith(
            _.arrival.head.getTime < _.arrival.head.getTime))
    } else {
      Left("Error - station lat/lon not found on the given route")
    }
  }

  var log = false
  var segSize = 0.1 // 100 meters
 
  val fudgeLat = 1.0/3600.0
  val fudgeLon = 1.0/3600.0

  /**
   * Find the smallest distance between the given route and
   * the given point
   *
   * If no distance less than a preset threshold can be found
   * this method returns None
   */
  //@test me
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
