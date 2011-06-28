import org.scalatest._
import org.scalatest.matchers._

import org.hinz.septa._
import org.hinz.gis._

import java.util.{Date,Calendar}



class EstimatorSpec extends Spec with ShouldMatchers {
  
  def createInterval(start: Double, end: Double, time: Double) =
    Interval(0, 0, 0, 0, start, end, null, time)

  def createIntervalWithV(start: Double, end: Double, v: Double) =
    createInterval(start, end, (end - start) / v)

  describe("Estimator") {
    it ("should reduce a list of intervals to a given speed and time") {
      val e = new Estimator()

      val dist = 10.0

      val ivals = List(
        createIntervalWithV(0.0,dist,5),
        createIntervalWithV(0.0,dist,10),
        createIntervalWithV(0.0,dist,15),
        createIntervalWithV(0.0,dist,20))

      e.getSpeedAndTime(10, ivals) should equal((12.5, dist / 12.5))
    }

    it ("should reduce an empty list of intervals to a speed of zero") {
      val e = new Estimator()

      e.getSpeedAndTime(0, Nil)._1 should equal(0)
    }

    it ("should be able to estimate a single interval with multiple models") {
      val e = new Estimator()

      val ivals = List(
        createIntervalWithV(0.0,10.0,5), 
        createIntervalWithV(10.0,20.0,10), 
        createIntervalWithV(20.0,30.0,15),
        createIntervalWithV(30.0,40.0,30))

      val ivals2 = List(
        createIntervalWithV(0.0,10.0,20), 
        createIntervalWithV(10.0,20.0,30),
        createIntervalWithV(10.0,20.0,100), 
        createIntervalWithV(20.0,30.0,55),
        createIntervalWithV(30.0,40.0,40))

      val model = new Model(0, 0.1, 1, (new Date(), None), (null,null), (0.0,100.0)) {
        override def isInDateRange(date: Date, date2: Date, date3: Date) = true
        override def isInTimeRange(date: Date, date2: Date, date3: Date) = true
      }

      val dist = 35.0 - 15.0
      val tgtspd = List(55.0/3.0, 225.0/4.0)
      val tgttime = tgtspd.map(dist/_)
      val eval = EstInterval(15.0,35.0, tgtspd.map(_*1000), tgttime)
      e.estimateInterval((15.0,35.0), List(ivals,ivals2), List(model,model)) should equal(eval)
    }

    it ("should be able to estimate the next bus") {
      pending
    }

    it ("should be able to estimate distance on a given route") {
      pending
    }
  }


}
