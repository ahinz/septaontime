package org.hinz.gis
import java.util.Date

import scala.annotation.tailrec

object U {
  def dblCompare(d1:Double,d2:Double,tol:Double = .0000000001) =
    math.abs(d1 - d2) < tol
}

/**
 * An interval represents the time it takes to go from one distance point
 * to another distance point
 *
 * Intervals also contain the date they were recorded
 */
case class Interval(start:Double, end:Double, time: Double, recordedAt: Date) {

  def velocity = (end - start)/time

  def samePoints(i: Interval) = 
    U.dblCompare(i.start,start) && U.dblCompare(i.end,end)

  /**
   * Determine if this interval contains the given point
   *
   * @param d point to check
   * @return true if d is in [start,end]
   */
  def contains(d: Double, includeEndPoints:Boolean = true):Boolean =
    if (includeEndPoints)
      d >= start && d <= end
    else
      d > start && d < end

  /**
   * Split an interval at the given points
   */
  def split(ds: List[Double]):List[Interval] =
    splith(this, ds.sortWith(_ < _))

  private def splith(i:Interval, ds: List[Double]):List[Interval] = ds match {
    case Nil => List(i)
    case x::xs => {
      val splitInterval = i.split(x)
      splitInterval._1 :: i.splith(splitInterval._2, xs)
    }
  }

  /**
   * Split an interval at the given point
   * The ratio of distance to time remains the same
   *
   * If d is not in the interval an exception is thrown
   */
  def split(d: Double):(Interval,Interval) =
    if (contains(d)) 
      (Interval(start, d, ((d - start)/(end - start))*time, recordedAt),
       Interval(d, end, ((end -d)/(end - start))*time, recordedAt))
    else
      throw new Exception("Cannot split " + toString + " on " + d)

  override def toString = "[" + start + ", " + end + " (" + time +")]"
}


/**
 * Utilities for working with intervals
 */
object Interval {

  /**
   * Given a distance (d) split any intervals that start before d on d
   *
   * @param startingPt the distance to split at
   * @param intervals the list of possible intervals
   * @return list with updated intervals
   */
  def matchStarts(startingPt: Double, intervals:List[Interval]):List[Interval] =
    intervals.map(interval =>
      if (interval.start < startingPt && startingPt < interval.end)
        interval.split(startingPt)._2
      else
        interval)

  /**
   * Given a distance (d) split any intervals that end after d on d
   *
   * @param endingPt the distance to split at
   * @param intervals the list of possible intervals
   * @return list with updated intervals
   */
  def matchEnds(endingPt: Double, intervals:List[Interval]):List[Interval] =
    intervals.map(interval =>
      if (interval.end > endingPt && endingPt > interval.start)
        interval.split(endingPt)._1
      else
        interval)

}

