import org.scalatest._
import org.scalatest.matchers._

import org.hinz.gis._      

class EstimatorSpec extends Spec with ShouldMatchers {
  describe("Estimator") {

    // Define combiner function is a simple average
    val cmb = (x:List[Interval]) => x.map(_.time).reduceLeft(_+_)/x.length.toDouble


    it ("should be able to interpolate missing segments") {
      val i1 = Interval(5,10,5,null)
      val i2 = Interval(10,20,10,null)
      val i3 = Interval(30,50,20,null)
      val i4 = Interval(60,70,10,null)

      new Estimator(null).interpolate(0.0,100.0, List(i1,i2,i3,i4)) should equal(List(
        Interval(0,5,5,null),
        Interval(5,10,5,null),
        Interval(10,20,10,null),
        Interval(20,30,10,null),
        Interval(30,50,20,null),
        Interval(50,60,10,null),
        Interval(60,70,10,null),
        Interval(70,100,30,null)))
    }

    it("should be able to estimate sub-segments with a single baseline") {
      val l = List(Interval(0,100,100,null))

      val e = new Estimator(cmb)

      e.estimate(20,30,l) should equal(10) // Inside
      e.estimate(90,110,l) should equal(20) // Overlap right
      e.estimate(-10,10,l) should equal(20) // Overlap left
    }

    it("should be able to esimate a compoud case") {
      val l = List(Interval(-200,-100,1000,null), // Ignore to left
                   Interval(-20,120,140,null), // Extends past left and right
                   Interval(20,30,30,null), // Normal interval in middle
                   Interval(80,130,10,null), // Extends right
                   Interval(120,200,3000,null)) // Ignore to right

      val e = new Estimator(cmb)

      e.estimate(40,50,l) should equal(10)
      e.estimate(0,100,l) should equal(102)

    }

    it("should be able to esimate a simple compound case") {
      val l = List(Interval(0,100,100,null),
                   Interval(20,30,30,null))

      val e = new Estimator(cmb)

      e.estimate(0,100,l) should equal(110)
      e.estimate(-10,10,l) should equal(20)
      e.estimate(90,110,l) should equal(20)
      e.estimate(-10,110,l) should equal(130)
    }
      
  }
}

class IntervalSpec extends Spec with ShouldMatchers {
  val is = List(
    Interval(10,20,0,null),
    Interval(12,22,0,null),
    Interval(15,24,0,null))


  describe("Interval utils") {
    
    it("should be able to interpolate between intervals") {
      val i1 = Interval(0,5,5,null)
      val i2 = Interval(10,20,30,null)
      
      Interval.interpolate(i1,i2) should equal(
        Interval(5,10,7.5,null))
    }

    it("should interpolate between interval and end point") {
      Interval.interpolate(Interval(5,10,5,null),20) should equal(
        Interval(10,20,10,null))

      Interval.interpolate(5,Interval(30,40,10,null)) should equal(
        Interval(5,30,25,null))
    }

    it("should be able to group intervals") {
      val l = List(
        Interval(0,5,5,null),
        Interval(5,10,6,null),
        Interval(5,10,7,null),
        Interval(10,15,8,null),
        Interval(15,20,9,null),
        Interval(15,20,4,null),
        Interval(20,25,3,null))

      Interval.group(l).sortWith(_.head.start < _.head.start) should equal(List(
        List(
          Interval(0,5,5,null)),
        List(
          Interval(5,10,6,null),
          Interval(5,10,7,null)),
        List(
          Interval(10,15,8,null)),
        List(
          Interval(15,20,9,null),
          Interval(15,20,4,null)),
        List(
          Interval(20,25,3,null))))
    }

    it("should be able to partition three intervals") {
      val l = List(
        Interval(0,10,10,null),
        Interval(5,25,20,null),
        Interval(15,20,5,null))

      Interval.partition(l).sortWith(_.start < _.start) should equal(List(
        Interval(0,5,5,null),
        Interval(5,10,5,null),
        Interval(5,10,5,null),
        Interval(10,15,5,null),
        Interval(15,20,5,null),
        Interval(15,20,5,null),
        Interval(20,25,5,null)))
    }
        

    it("should be able to partition three intervals (non overlapping)") {
      val l = List(
        Interval(10,20,10,null),
        Interval(20,30,10,null),
        Interval(30,40,10,null))

      Interval.partition(l) should equal(l)
    }

    it("should be able to partition two intervals") {
      val i1 = Interval(10,20,10,null)
      val i2 = Interval(15,25,10,null)

      Interval.partition(List(i1,i2)).sortWith(_.start < _.start) should equal(List(
        Interval(10,15,5,null),
        Interval(15,20,5,null),
        Interval(15,20,5,null),
        Interval(20,25,5,null)))
    }

    it("should be able to partition a single interval") {
      var l = List(Interval(10,20,10,null))
      Interval.partition(l) should equal(l)
    }

    it("should be able to trim a set of items based on the starting point") {

      val m = Interval.matchStarts(13,is)

      m(0).start should equal(13)
      m(1).start should equal(13)
      m(2).start should equal(15)
    }

    it("should be able to trim off the end of items based on the ending point") {
      val m = Interval.matchEnds(21,is)

      m(0).end should equal(20)
      m(1).end should equal(21)
      m(2).end should equal(21)
    }
  }

  describe("An interval") {

    it("should be able to split with a list of indicies") {
      var l1 = Interval(1,10,9,null)

      l1.split(List(2.0,9.0,3.0)) should equal(List(
        Interval(1,2,1,null),
        Interval(2,3,1,null),
        Interval(3,9,6,null),
        Interval(9,10,1,null)))
    }

    it("should partition overlaps") {
      // No overlap
      var l1 = Interval(1,10,10,null)
      var l2 = Interval(10,20,10,null)

      l1.partition(l2).sortWith(_.start < _.start) should equal(List(l1,l2))
   
      // Left overlap
      l1 = Interval(20,30,10,null)
      l2 = Interval(15,25,10,null)

      l1.partition(l2).sortWith(_.start < _.start) should equal(List(
        Interval(15,20,5,null),
        Interval(20,25,5,null),
        Interval(20,25,5,null),
        Interval(25,30,5,null)))
          
      // Right overlap
      l1 = Interval(20,30,10,null)
      l2 = Interval(25,35,10,null)

      l1.partition(l2).sortWith(_.start < _.start) should equal(List(
        Interval(20,25,5,null),
        Interval(25,30,5,null),
        Interval(25,30,5,null),
        Interval(30,35,5,null)))
     
      // l1 inside l2
      l1 = Interval(20,30,10,null)
      l2 = Interval(10,40,30,null)
     
      l1.partition(l2).sortWith(_.start < _.start) should equal(List(
        Interval(10,20,10,null),
        Interval(20,30,10,null),
        Interval(20,30,10,null),
        Interval(30,40,10,null)))

      // l2 inside l1
      l1 = Interval(10,40,30,null)
      l2 = Interval(20,30,10,null)
     
      l1.partition(l2).sortWith(_.start < _.start) should equal(List(
        Interval(10,20,10,null),
        Interval(20,30,10,null),
        Interval(20,30,10,null),
        Interval(30,40,10,null)))

      // Perfect overlap
      l1 = Interval(10,20,10,null)
      l2 = Interval(10,20,10,null)

      l1.partition(l2).sortWith(_.start < _.start) should equal(List(
        Interval(10,20,10,null),
        Interval(10,20,10,null)))

    }

    it("should properly implement contains") {
      val i = Interval(10,20,10,null)

      i.contains(0) should equal(false)
      i.contains(10) should equal(true)
      i.contains(11) should equal(true)
      i.contains(20) should equal(true)
      i.contains(21) should equal(false)
    }

    it("should properly implement contains without endpoints") {
      val i = Interval(10,20,10,null)

      i.contains(0,false) should equal(false)
      i.contains(10,false) should equal(false)
      i.contains(11,false) should equal(true)
      i.contains(20,false) should equal(false)
      i.contains(21,false) should equal(false)
    }

    it ("should split properly") {
      val i = Interval(1,9,9,null)
      val is = i.split(4)

      val i1 = is._1
      val i2 = is._2

      i1.start should equal (i.start)
      i1.end should equal (4)
      i1.time should equal (3.375)
      i2.start should equal (i1.end)
      i2.end should equal (i.end)
      i2.time should equal (9.0 - 3.375)
      
    }
  }
  

}
