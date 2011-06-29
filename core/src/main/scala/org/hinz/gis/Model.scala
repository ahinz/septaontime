package org.hinz.gis

import org.hinz.septa._
import java.util.{Date,Calendar}


/**
 * Convience object for building modesl
 */
object Model {
  def now = nowDate.getTime
  def nowDate = new Date()

  val day = 1000 * 60 * 60 * 24

  def timeHoursAgo(hours: Int, minutes: Int = 0) = {
    val cal = Calendar.getInstance()
    val now = new Date()
    cal.setTime(now)
    cal.set(Calendar.HOUR_OF_DAY, cal.get(Calendar.HOUR_OF_DAY) - hours)
    cal.set(Calendar.MINUTE, cal.get(Calendar.MINUTE) - minutes)

    (cal.getTime(), now)
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
