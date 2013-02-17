package tests

import stream._

import scala.util.{ Random => Random }
import org.scalatest.FunSuite


class Stream extends FunSuite {
  test("Test ingestDoc for particle filter-based LDA") {
    val r = new Random()
    
    def randomTest (setSz: Int, lstSz: Int): AssociativeStreamSampler[Int] = {
      def loop (i: Int, accu: AssociativeStreamSampler[Int]):
      AssociativeStreamSampler[Int] = {
	if (i == lstSz) accu
	else {
	  accu.add(r.nextInt(9))
	  loop(i+1, accu)
	}
      }
      var rs = new ReservoirSampler[Int](setSz)
      loop(0, rs)
    }
    
    println(randomTest(5, 15).getSampleSet().deep.mkString(" "))
    println(randomTest(5, 15).getSampleSet().deep.mkString(" "))
    println(randomTest(5, 15).getSampleSet().deep.mkString(" "))
    println(randomTest(5, 15).getSampleSet().deep.mkString(" "))
    println(randomTest(5, 15).getSampleSet().deep.mkString(" "))
    println(randomTest(5, 15).getSampleSet().deep.mkString(" "))
    println(randomTest(5, 15).getSampleSet().deep.mkString(" "))
    println(randomTest(5, 15).getSampleSet().deep.mkString(" "))
  }
}
