package org.hinz.gis

import org.hinz.septa._
import java.util.{Date,Calendar}
import java.awt.geom.Point2D

import scala.annotation.tailrec

case class LatLon(lat:Double,lon:Double) {
  def toPoint2D = new Point2D.Double(lon,lat)
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
      printlg("spd @ ival: " + spd + " | " + ivals.size + " | " + ivals.take(3))

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
  def reduceIntervalsToMinuteOffset(evals: List[EstInterval]):List[Double] =
    reduceIntervalsToMinuteOffset(evals.map(_.t))

  @tailrec
  final def reduceIntervalsToMinuteOffset(evals: List[List[Double]],times:List[Double] = Nil):List[Double] = 
    if (evals.size == 0 || evals.head.size == 0) times.reverse
    else {
      var split = evals.map(v => (v.head, v.tail))
      var cur = split.map(_._1)
      var rest = split.map(_._2)
      reduceIntervalsToMinuteOffset(rest, (cur.reduceLeft(_+_) / 60.0) :: times)
    }


  def createBusEst(station: LatLon, sref: Double, r: BusRecord, dist: Option[Double]):Option[BusEst] = dist match {
    case Some(aDist) if aDist >= 0 && aDist <= sref => Some(BusEst(
      r.BlockID,
      r.VehicleID,
      station,
      r.Offset.toDouble,
      aDist,
      null))
    case _ => None
  }
      
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
  def estimateNextBus(station: LatLon, route:List[RoutePoint], buses: List[BusRecord], models: List[Model], ivals:List[Interval]):Option[List[BusEst]] = {
    
    // Determine linear ref for the station
    var nearestPt = nearestPointOnRoute(route, station)

    if (nearestPt.isDefined) {
      val sref = nearestPt.get.distanceTo(station)

      // Convert each bus to a linear ref and discard busses
      // that have already arrived at the destination
      val brefs:List[BusEst] = buses.flatMap(bus =>
        createBusEst(station, 
                     sref, 
                     bus, 
                     nearestPointOnRoute(
                       route, 
                       bus.toLatLon).map(
                         _.distanceTo(station))))
      
      // Convert from time offset (in seconds) to a data
      // also substract original delay (in minutes)
      Some(brefs.map(x => 
        x.arrival(
	  reduceIntervalsToMinuteOffset(
            estimateIntervals(models.map(
              _.copyWithNewDistances(x.offset, sref)), ivals)).map(t => 
                new Date((t.toLong - x.origOffset.toInt)* 60*1000L + now)))).sortWith(
                  _.arrival.head.getTime < _.arrival.head.getTime))
    } else {
      None
    }
  }
    
/*
  def estimateTimeBetweenPoints(pt1: LatLon, pt2:LatLon, route: List[RoutePoint], models:List[Model], ivals:List[Interval]):Option[List[EstInterval]] = {
    val pt1distOpt = nearestPointOnRoute(route, pt1).map(_.distanceTo(pt1))
    val pt2distOpt = nearestPointOnRoute(route, pt2).map(_.distanceTo(pt2))
    
    if (pt1distOpt.isDefined && pt2distOpt.isDefined)
      Some(estimateIntervals(
        models.map(_.copyWithNewDistances(pt1distOpt.get, pt2distOpt.get)),
        ivals))
    else
      None
  }
*/
  var log = false
  var segSize = 0.1 // 100 meters
 
  // Tune these as you wish
  var fudgeLat = 1.0/3600.0
  var fudgeLon = 1.0/3600.0
  var minDist = 0.01

  /**
   * Find the smallest distance between the given route and
   * the given point
   *
   * If no distance less than a preset threshold can be found
   * this method returns None
   */
  //@test me
  def nearestPointOnRoute(route: List[RoutePoint], pt:LatLon):Option[RoutePoint] = route match {
    case Nil => None
    case _ =>
      nearestPointOnRoute(route.zip(route.tail), pt, None, minDist)
  }


  @tailrec
  final def nearestPointOnRoute(route: List[(RoutePoint,RoutePoint)], tgtPt: LatLon, minRt: Option[RoutePoint], minDist: Double):Option[RoutePoint] = route match {
    case Nil => minRt
    case x::xs => {
      if (boundingBoxContainsPt(x._1,x._2,tgtPt)) {
        var newMinDist = GIS.minDistance((tgtPt.lon,tgtPt.lat), 
                                         GIS.computeLine(x._1.lon,x._1.lat,x._2.lon,x._2.lat))

        if (newMinDist <= minDist)
          nearestPointOnRoute(xs, tgtPt, Some(x._1), newMinDist)
        else
          nearestPointOnRoute(xs, tgtPt, minRt, minDist)
      } else
        nearestPointOnRoute(xs, tgtPt, minRt, minDist)
    }  
  }
                    
  def boundingBoxContainsPt(p1: RoutePoint, p2: RoutePoint, pt: LatLon, fudgeLat:Double = fudgeLat, fudgeLon:Double = fudgeLon) = {
    val maxlat = p1.lat.max(p2.lat)
    val minlat = p1.lat.min(p2.lat)
    val maxlon = p1.lon.max(p2.lon)
    val minlon = p1.lon.min(p2.lon)
    
    pt.lat >= minlat - fudgeLat  && pt.lat <= maxlat + fudgeLat &&
    pt.lon >= minlon - fudgeLon && pt.lon <= maxlon + fudgeLon
  }
  
/*
  def distanceOnRoute(route: List[RoutePoint], pt:LatLon):Option[Double] = {
    
    // Determine if any route point pair could contain this interval
    val minDist = 0.01

    val m =
      route.zip(route.tail).foldLeft((None:Option[(RoutePoint,RoutePoint)],minDist))((curmin,p) => {
        val p1 = p._1
        val p2 = p._2

        if (boundingBoxContainsPt(p1,p2,pt,fudgelat, fudgeLon)) {
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
*/
  def printlg(x:String) = if (log) print(x)
  def printlnlg(x:String) = if (log) println(x)

}
