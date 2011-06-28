import org.scalatest._
import org.scalatest.matchers._

import org.hinz.septa._
import org.hinz.gis._

import java.util.{Date,Calendar}

class ModelSpec extends Spec with ShouldMatchers {
  describe("Model") {
    it ("should generate properly spaced intervals") {
      val model = Model(0,1,2,(null,null),(null,null),(0.0,50.0))

      model.createOutputIntervals() should equal((0 to 49).zip(1 to 50).toList)
    }
  }

  it ("should throw an exception for negative or inverted ranges") {
    val model = Model(0,1,2,(null,null),(null,null),(-1.0,50.0))
    evaluating { model.createOutputIntervals() } should produce [IllegalArgumentException]

    val model2 = Model(0,1,2,(null,null),(null,null),(50.0,10.0))
    evaluating { model2.createOutputIntervals() } should produce [IllegalArgumentException]
  }

  it ("should handle throw an exception for negative or zero seg size") {
    val model = Model(0,0,2,(null,null),(null,null),(0.0,50.0))
    evaluating { model.createOutputIntervals() } should produce [IllegalArgumentException]

    val model2 = Model(0,-1,2,(null,null),(null,null),(0.0,50.0))
    evaluating { model2.createOutputIntervals() } should produce [IllegalArgumentException]
  }

  it ("should properly handle date ranges") {
    val now = new Date()
    val date1 = new Date(now.getTime() - 1000*60*60*24)
    val date2 = new Date(now.getTime() + 1000*60*60*24)

    val model = Model(0,1,2,(date1,Some(date2)),(null,null),(0.0,50.0))    
    model.isInDateRange(now,date1,date2) should equal(true)
    model.isInDateRange(date1,now,date2) should equal(false)
    model.isInDateRange(date2,date1,now) should equal(false)
  }
 
  it ("should properly handle time ranges") {
    val cal = Calendar.getInstance()
    cal.set(Calendar.HOUR_OF_DAY, 5)
    cal.set(Calendar.MINUTE, 20)

    val date1 = cal.getTime()

    cal.set(Calendar.HOUR_OF_DAY, 5)
    cal.set(Calendar.MINUTE, 40)
    
    val date2 = cal.getTime()

    cal.set(Calendar.HOUR_OF_DAY, 20)
    
    val date3 = cal.getTime()

    val model = Model(0,1,2,(date1,Some(date2)),(null,null),(0.0,50.0))    
    model.isInTimeRange(date2,date1,date3) should equal(true)
    model.isInTimeRange(date3,date1,date2) should equal(false)
    model.isInTimeRange(date1,date2,date3) should equal(false)

  }

  it("should use date and time ranges to determine if a given interval is used") {
    var dateOk = false
    var timeOk = false

    val model = new Model(0,1,2,(new Date(),Some(new Date())),(null,null),(10.0,50.0)) {
      override def isInDateRange(d: Date, startDate: Date, endDate: Date):Boolean = dateOk
      override def isInTimeRange(d: Date, startTime: Date, endTime: Date):Boolean = timeOk
    }

    val model2 = new Model(0,1,2,(new Date(),None),(null,null),(10.0,50.0)) {
      override def isInDateRange(d: Date, startDate: Date, endDate: Date):Boolean = dateOk
      override def isInTimeRange(d: Date, startTime: Date, endTime: Date):Boolean = timeOk
    }

    val interval1 = Interval(0,0,0,0,1.0,7.0, new Date(), 0)
    val interval2 = Interval(0,0,0,0,1.0,12.5, new Date(), 0)
    val interval3 = Interval(0,0,0,0,12.7,40.0, new Date(), 0)
    val interval4 = Interval(0,0,0,0,42.0,70.0, new Date(), 0)
    val interval5 = Interval(0,0,0,0,62.7,80.0, new Date(), 0)

    model.usesInterval(interval1) should equal(false)

    timeOk = true
    model.usesInterval(interval1) should equal(false)

    model2.usesInterval(interval3) should equal(true)

    dateOk = true
    model.usesInterval(interval1) should equal(false)

    model.usesInterval(interval2) should equal(true)
    model.usesInterval(interval3) should equal(true)
    model.usesInterval(interval4) should equal(true)

    model.usesInterval(interval5) should equal(false)
  }
    
}
