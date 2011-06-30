import org.scalatest._
import org.scalatest.matchers._

import org.hinz.septa._
import org.hinz.gis._

import java.util.{Date,Calendar}



class EstimatorSpec extends Spec with ShouldMatchers {
  
  def createInterval(start: Double, end: Double, time: Double) =
    Interval(0, 0, 0, 0, start, end, new Date(new Date().getTime() + 1000), time) // Add some time so we always fall in the date range

  def createIntervalWithV(start: Double, end: Double, v: Double) =
    createInterval(start, end, (end - start) / v)

  describe("Estimator") {

    describe("Speed calculation") {
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
      
      it ("should be able to handle bad data resulting in a zero speed") {
        val e = new Estimator()
        
        e.getSpeedAndTime(10, List(
          createIntervalWithV(0.0,10.0,0),
          createIntervalWithV(0.0,10.0,0),
          createIntervalWithV(0.0,10.0,0),
          createIntervalWithV(0.0,10.0,0)))._1 should equal(0)
      }
    }

    describe("interval estimation") {
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
    }

    describe("Reduction") {
      it("should be able to reduce zero intervals to a valid date") {
        val e = new Estimator()
        e.reduceIntervalsToMinuteOffset(Nil) should equal(Nil)
        e.reduceIntervalsToMinuteOffset(List(Nil)) should equal(Nil)
      }

      it("should be able to reduce simple intervals") {
        val nowDate = new Date().getTime()
        val e = new Estimator() {
          override def now = nowDate
        }

        val minOffset = 12
        val minOffsetMils = - 1000*60 * minOffset

        // Times are in seconds
        e.reduceIntervalsToMinuteOffset(
          List(List(5.0))) should equal(List(5.0/60.0))

      }

      it("should be able to reduce multiple intervals") {
        val nowDate = new Date().getTime()
        val e = new Estimator() {
          override def now = nowDate
        }

        val list1 = List(5.0,2.0,5.0,4.0,2.2,3.1)
        val list1sum = list1.reduceLeft(_+_) / 60.0
        val list2 = List(1.2,5.0,2.4,9.9,2.5,2.2) // 6 elmts
        val list2sum = list2.reduceLeft(_+_) / 60.0
        val lst:List[List[Double]] = for(j <- (0 until list1.size).toList)
          yield List(list1(j), list2(j))

        val minOffset = 15
        val minOffsetMils = - 1000*60 * minOffset

        // Times are in seconds
        e.reduceIntervalsToMinuteOffset(lst) should equal(List(list1sum, list2sum))
      }

    }

    def mkRoutePt(lat: Double, lon: Double, ref: Double) =
      RoutePoint(0, 0, lat, lon, ref)

    def mkBusRec(lat: Double, lon: Double, offset: String = "0") =
      BusRecord(lat.toString, lon.toString, null, null, null, null, null, offset)

    describe("Route matching") {
      it ("should be able to tell if a point is in the bounding box") {
        val e = new Estimator()
        e.fudgeLat = 0
        e.fudgeLon = 0

        e.boundingBoxContainsPt(mkRoutePt(10,20,0), mkRoutePt(20,40,0), LatLon(5, 5)) should equal(false)
        e.boundingBoxContainsPt(mkRoutePt(10,20,0), mkRoutePt(20,40,0), LatLon(5, 25)) should equal(false)
        e.boundingBoxContainsPt(mkRoutePt(10,20,0), mkRoutePt(20,40,0), LatLon(15, 5)) should equal(false)
        e.boundingBoxContainsPt(mkRoutePt(10,20,0), mkRoutePt(20,40,0), LatLon(10, 19)) should equal(false)

        e.boundingBoxContainsPt(mkRoutePt(10,20,0), mkRoutePt(20,40,0), LatLon(10, 20)) should equal(true)
        e.boundingBoxContainsPt(mkRoutePt(10,20,0), mkRoutePt(20,40,0), LatLon(20, 20)) should equal(true)
        e.boundingBoxContainsPt(mkRoutePt(10,20,0), mkRoutePt(20,40,0), LatLon(20, 40)) should equal(true)
        e.boundingBoxContainsPt(mkRoutePt(10,20,0), mkRoutePt(20,40,0), LatLon(15, 30)) should equal(true)

        e.fudgeLat = 5
        e.fudgeLon = 5

        e.boundingBoxContainsPt(mkRoutePt(10,20,0), mkRoutePt(20,40,0), LatLon(5, 5)) should equal(false)
        e.boundingBoxContainsPt(mkRoutePt(10,20,0), mkRoutePt(20,40,0), LatLon(5, 25)) should equal(true)
        e.boundingBoxContainsPt(mkRoutePt(10,20,0), mkRoutePt(20,40,0), LatLon(15, 45)) should equal(true)
        e.boundingBoxContainsPt(mkRoutePt(10,20,0), mkRoutePt(20,40,0), LatLon(10, 19)) should equal(true)
      }

      it ("should be able to calculate distance on empty route list") {
        val e = new Estimator()
        e.nearestPointOnRoute(Nil, LatLon(2,2)) should equal(None)
        e.nearestPointOnRoute(Nil, LatLon(2,2), None, 0.0) should equal(None)
      }

      it ("should be able to determine a simple case") {
        val e = new Estimator()
        e.fudgeLat = 2.0
        e.fudgeLon = 2.0
        e.minDist = 2.0

        // Cheat and use flat coord system
        GIS.distanceCalculator = GIS.flatDistanceCalculator

        val p1 = mkRoutePt(1, 0, 100)
        val p2 = mkRoutePt(2, 10, 200)
        val p3 = mkRoutePt(3, 25, 250)
        val p4 = mkRoutePt(4, 40, 400)
        val p5 = mkRoutePt(5, 41, 500)
        val p6 = mkRoutePt(6, 43, 510)
        val p7 = mkRoutePt(-2, -2, 700)

        val pts = List(p1,p2,p3,p4, p5, p6, p7)

        e.nearestPointOnRoute(pts, LatLon(0,43)) should equal(None) // Out of bounds
        e.nearestPointOnRoute(pts, LatLon(3,25)) should equal(Some(p3)) // Right on pt
        e.nearestPointOnRoute(pts, LatLon(2.6,17.5)) should equal(Some(p2)) // Between pts
        e.nearestPointOnRoute(pts, LatLon(4.7,41.0)) should equal(Some(p4)) // Smaller dist (don't jump ahead)

        e.minDist = 30.0
        e.nearestPointOnRoute(pts, LatLon(3,25)) should equal(Some(p3)) // Ignore others after smallest found

      }
    }

    describe("bus times estimates") {
      it ("should be able to fail gracefully if a station, or bus is not on the route") {
        // Cheat and use flat coord system
        GIS.distanceCalculator = GIS.flatDistanceCalculator

        val e = new Estimator()

        val model = null

        // Bad station
        e.estimateNextBus(LatLon(10,10), 
                          List(mkRoutePt(200,200,10),mkRoutePt(250,250,20)),
                          List(mkBusRec(200,200)),
                          List(model),
                          Nil) should equal(None)

      }

      it ("should properly estimate a simple simulation") {
        val e = new Estimator()
        val m = new Model(0, 10.0, 0, (new Date(0L), None), Model.time24h, (0.0,100.0))

        // Cheat and use flat coord system
        GIS.distanceCalculator = GIS.flatDistanceCalculator

        val busRec = mkBusRec(0,10)
        val route = List(
          mkRoutePt(0,10,10),
          mkRoutePt(1,100,100))

        val ivals = List(
          createInterval(0,10,10),
          createInterval(10,20,10),
          createInterval(20,30,10),
          createInterval(30,40,10),
          createInterval(40,50,10),
          createInterval(50,60,10),
          createInterval(60,70,10),
          createInterval(70,80,10),
          createInterval(80,90,10),
          createInterval(90,100,10))

        val est = e.estimateNextBus(LatLon(1,98), 100.0, busRec, route, List(m), ivals)

        println(est)
        est.isDefined should equal(true)
        est.get.arrival should equal(List(1.5))
      }

      it ("should properly estimate a more complex simulation") {
        pending
      }
    }

  }
}
