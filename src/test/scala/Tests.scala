package tests

import gibbs._
import org.scalatest.FunSuite
import java.util.{ Arrays => Arrays }

class StatsTests extends FunSuite {
  test("normalize") {
    val arr0 = Array[Double]()
    val arr1 = Array[Double](3)
    val arr2 = Array[Double](0.2)
    val arr3 = Array[Double](1, 1, 1, 1)
    val arr4 = Array[Double](1, 1, 1)
    
    assert(Arrays.equals(Stats.normalize(arr0), Array[Double]()),
	   "array should be empty")
    assert(Arrays.equals(Stats.normalize(arr1), Array[Double](1)),
	   "array should be unit array")
    assert(Arrays.equals(Stats.normalize(arr2), Array[Double](1)),
	   "array should be unit array")
    assert(Arrays.equals(Stats.normalize(arr3),
			 Array(0.25, 0.5, 0.75, 1.0)),
	 "elements are equiprobable")
    assert(Arrays.equals(Stats.normalize(arr4),
			 Array(1.0/3.0, 2.0/3.0, 1)),
	   "elements are equiprobable")
  }
}

class GibbsTest extends FunSuite { }
