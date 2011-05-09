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

  private def explodePair[T](t: (T,T)) = List(t._1,t._2)

  /**
   * Given this (T) and another interval I partition this such that
   * there are four connected intervals that do not overlap, if T and I overlap.
   * If they don't, return T and I
   */
  def partition(i: Interval):List[Interval] = {
    if (i.end <= start || i.start >= end) // No overlap
      List(i,this)
    else if (U.dblCompare(i.end,end) && U.dblCompare(i.start,start)) // Same
      List(i,this)
    else if (i.start > start && i.start < end && i.end > end) // Left overlap
      explodePair(split(i.start)) ++ explodePair(i.split(end))
    else if (i.start < start && i.end > start && i.end < end) // Right overlap
      i.partition(this)
    else if (i.start > start && i.end < end) { // i inside this
      var first = split(i.start)
      var rest = explodePair(first._2.split(i.end))
      first._1 :: i :: rest
    } else { // this inside i
      i.partition(this)
    }
  }

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
 * Used to estimate time between two distance points based
 * on a set of measured intervals
 *
 * @param combiner a function that takes a list of intervals
 * that all represent the same interval but with different time data
 * and computes an estimate time for that interval
 */
class Estimator(val combiner:(List[Interval] => Double)) {
  
  var log = false

  private def doCombine(l:List[Interval]):Interval = l match {
    case Nil => throw new Exception("Cannot combine empty list")
    case x::xs => Interval(x.start, x.end, combiner(l), null)
  }

  /**
   * Given a list of intervals, interpolate any intervals
   * that are empty between p1 and p2
   *
   * @param p1 Starting point
   * @param p2 Ending point
   * @param lst the list of intervals between p1 and p2
   * @return an unbroken string of intervals from p1 to p2
   */
  def interpolate(p1:Double, p2:Double, lst:List[Interval]):List[Interval] = {
    def interpolateh(lst:List[Interval], acc:List[Interval] = Nil):List[Interval] = lst match {
      case x::xs::xss => {
        if (U.dblCompare(x.end,xs.start)) 
          interpolateh(xs :: xss, x :: acc)
        else {
          println("Had to interp between " + x + " and " + xs)
          interpolateh(Interval.interpolate(x,xs) :: xs :: xss, x :: acc)
        }
      }
      case Nil => acc.reverse
      case x::xs => 
        if (x.end < p2) interpolateh(xs, Interval.interpolate(x, p2) :: x :: acc)
        else interpolateh(xs, x :: acc)
    }
    
    if (lst.length == 0) throw new Exception("Can't interpolate if NO datapoints are available")

    if(lst.head.start > p1) interpolate(p1,p2,Interval.interpolate(p1,lst.head) :: lst)
    else interpolateh(lst.sortWith(_.start < _.start))
  }

  var segSize = 0.1 // 100 meters
 
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
    

  def estimateOld(startDist:Double, endDist:Double, intervals:List[Interval]):Double = {
    // Get rid of parts that start before/end after the useful area
    val editedIntervals = Interval.matchEnds(endDist,
                                             Interval.matchStarts(startDist, intervals)).filter(x =>
                                               x.end >= startDist && x.start <= endDist)

    // Partition and group
    var groups:List[List[Interval]] = Interval.group(Interval.partition(editedIntervals))

    // Apply estimator
    var estimated:List[Interval] = groups.map(doCombine(_)).filter(_.time > 0.0).filter(x => x.start != x.end)

    if (log) {
      println("Groups: " + groups)
      println("Est: " + estimated)
      println("Intervals: " + editedIntervals)
    }

    // Interpolate
    // Sort before we interpolate
    val sorted:List[Interval] = estimated.sortWith(_.start < _.start)

    println(sorted.take(4))
    var interpolated:List[Interval] = interpolate(startDist, endDist, sorted)

    if (log) {
      println("Intr: " + interpolated)
    }

    // Sum up intervals
    interpolated.foldLeft(0.0)(_ + _.time)
  }
}

/**
 * Utilities for working with intervals
 */
object Interval {

  /**
   * Given i1.end < i2.start compute an interval
   * that connects i1 and i2 with a time based on the
   * avg velocity of i1 and i2
   *
   * @param i1 first interval
   * @param i2 second interval
   */
  def interpolate(i1:Interval,i2:Interval) = {
    val v1 = (i1.end - i1.start)/i1.time
    val v2 = (i2.end - i2.start)/i2.time
    val avg = (v1 + v2)/2.0 

    val newDist = i2.start - i1.end

    if (newDist <= 0) throw new Exception("Cannot interpolate an interval between " + i1 + " and " + i2)

    Interval(i1.end, i2.start, newDist / avg, i1.recordedAt)
  }

  def interpolate(i1:Interval, d:Double):Interval = 
    if (d > i1.end)
      Interval(i1.end, d, i1.time / (i1.end - i1.start) * (d - i1.end), i1.recordedAt)
    else
      i1

  def interpolate(d:Double, i1:Interval):Interval =
    if (d < i1.start)
      Interval(d, i1.start, i1.time / (i1.end - i1.start) * (i1.start - d), i1.recordedAt)
    else
      i1
  
  
  
  /**
   * Given a bunch of intervals, partition each interval so that
   * no intervals overlap and the sum of all interval time is the same
   *
   */
  def partition(intervals:List[Interval]):List[Interval] = {
    // Create a list of all split points:
    val points:List[Double] = intervals.flatMap(i => List(i.start,i.end)).distinct

    intervals.flatMap(i => i.split(points.filter(i.contains(_,false))))
  }
  
  /**
   * Given a bunch of intervals group by the same start and end points
   *
   * @param intervals the list of intervals
   * @return List of intervals where each sublist contains intervals
   * with matching start and end points
   */
  def group(intervals:List[Interval]):List[List[Interval]] = intervals match {
    case Nil => Nil
    case x::xs => {
      val parts = xs.partition(_.samePoints(x))
      (x :: parts._1) :: group(parts._2)
    }
  }

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

