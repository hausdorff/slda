package tests

import stream._

import scala.util.{ Random => Random }
import java.util.{ Arrays => Arrays }
import org.scalatest.FunSuite


class Stream extends FunSuite {
  /** feed it a function that throws an exception when called, and we will
   check to see that it does indeed */
  def assertExceptionCaught [T](func: () => T): Unit = {
    var exceptionCaught = false
    try {
      func()
    }
    catch {
      case e: Exception => exceptionCaught = true
    }
    assert(exceptionCaught)
  }

  def randomTest2 (setSz: Int, lstSz: Int): AssociativeStreamSampler[Int] = {
    val r = new Random()
    def loop (i: Int, accu: AssociativeStreamSampler[Int]):
    AssociativeStreamSampler[Int] = {
      if (i == lstSz) accu
      else {
	accu.add(r.nextInt(4))
	loop(i+1, accu)
      }
    }
    var rs = new ReservoirSampler[Int](setSz)
    rs.addAll(Array(0,0,0))
    loop(0, rs)
  }

  test("sanity check -- make samples *look* good") {
    println(randomTest2(4,150).getSampleSet().deep.mkString(" "))
    println(randomTest2(4,150).getSampleSet().deep.mkString(" "))
    println(randomTest2(4,150).getSampleSet().deep.mkString(" "))
    println(randomTest2(4,150).getSampleSet().deep.mkString(" "))
    println(randomTest2(4,150).getSampleSet().deep.mkString(" "))
    println(randomTest2(4,150).getSampleSet().deep.mkString(" "))
    println(randomTest2(4,150).getSampleSet().deep.mkString(" "))
    println(randomTest2(4,150).getSampleSet().deep.mkString(" "))
  }

  test("make sure some of the time, elements get rejected from sample") {
    var count = 0
    (0 to 10000).foreach { i =>
      var sampler = new ReservoirSampler[Int](1)
      assert(sampler.addItem(1) == 0)
      if (sampler.addItem(1) == -1) count += 1 }
    assert(count < 6000 && count > 4000, "sampler failed a statistical test")

    count = 0
    (0 to 10000).foreach { i =>
      var sampler = new ReservoirSampler[Int](2)
      assert(sampler.addItem(1) == 0)
      assert(sampler.addItem(1) == 1)
      if (sampler.addItem(1) == -1) count += 1 }
    assert(count < 4000 && count > 2000, "sampler failed a statistical test")
  }
  
  test("Test reservoir sampler") {
    val r = new Random()
    
    def randomTest (setSz: Int, lstSz: Int): AssociativeStreamSampler[Int] = {
      def loop (i: Int, accu: AssociativeStreamSampler[Int]):
      AssociativeStreamSampler[Int] = {
	if (i == lstSz) accu
	else {
	  accu.add(r.nextInt(2))
	  loop(i+1, accu)
	}
      }
      var rs = new ReservoirSampler[Int](setSz)
      rs.addAll(Array(0,0,0))
      loop(0, rs)
    }

    var test1 = new ReservoirSampler[Int](3)
    test1.addAll(Array(1,2,3))
    val target1 = Array[Int](1,2,3)
    assert(test1.getSampleSet().deep == target1.deep)
    assert(test1.occupied == 3)
    assert(test1(0) == 1)
    assert(test1(1) == 2)
    assert(test1(2) == 3)
    
    var hetero = 0.0
    var n = 100000
    val targetlowerbound = 40000
    val targetupperbound = 60000
    for (i <- 0 to n) {
      val curr = randomTest(2, 150).getSampleSet()
      if ((curr(0) == 0 && curr(1) == 1) || (curr(0) == 1 && curr(1) == 0))
	hetero += 1
    }
    println(hetero)
    assert(hetero/n < targetlowerbound || hetero/n > targetupperbound,
	   "reservoir sample failed statistical test!")

    var test2 = new ReservoirSampler[Int](3)
    val target2 = Array[Int]()
    test2.addAll(Array())
    assert(test2.occupied == 0)
    assert(test2.getSampleSet.deep == target2.deep)
    assertExceptionCaught{ () => test2(0) }
    assertExceptionCaught{ () => test2(1) }
    val target3 = 1
    test2.add(target3)
    assert(test2(0) == target3)
    assertExceptionCaught{ () => test2(1) }

    var test3 = new ReservoirSampler[Int](3)
    assert(test3.occupied == 0)
    test3.add(1)
    assert(test3.occupied == 1)
    val target4 = Array[Int](1)
    assert(test3.getSampleSet.deep == target4.deep)
    test3.add(2)
    assert(test3.occupied == 2)
    val target5 = Array[Int](1,2)
    assert(test3.getSampleSet.deep == target5.deep)
    val target6 = Array[Int](1,2,3)
    test3.add(3)
    assert(test3.occupied == 3)
    assert(test3.getSampleSet.deep == target6.deep)
    test3.add(4)
    assert(test3.occupied == 3)
  }
}
