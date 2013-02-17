package tests

import stream._

import scala.util.{ Random => Random }
import java.util.{ Arrays => Arrays }
import org.scalatest.FunSuite


class Stream extends FunSuite {
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
    println(test1.getSampleSet().mkString(" "))
    assert(test1.getSampleSet().deep == target1.deep)
    assert(test1.size == 3)
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
  }
}
