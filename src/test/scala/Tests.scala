package tests

import gibbs._
import wrangle._
import org.scalatest.FunSuite
import java.util.{ Arrays => Arrays }

class StatsTests extends FunSuite {
  test("normalize") {
    val arr0 = Array[Double]()
    val arr1 = Array[Double](3)
    val arr2 = Array[Double](0.2)
    val arr3 = Array[Double](1, 1, 1, 1)
    val arr4 = Array[Double](1, 1, 1)
    val arr5 = Array[Double](1, 1)
    val arr6 = Array[Double](2, 1)
    
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
    assert(Arrays.equals(Stats.normalize(arr5), Array(0.5, 1)),
	   "elements are equiprobable")
    assert(Arrays.equals(Stats.normalize(arr6), Array(2.0/3.0, 1)))
  }

  test("sampleCategorical") {
    val arr1 = Array[Double](2, 1)
    val cdf = Stats.normalize(arr1)
    var one = 0
    var two = 0.0
    for (i <- 0 to 10000000) {
      val res = Stats.sampleCategorical(cdf)
      if (res == 0) one += 1
      else two += 1
    }
    assert(one/(one+two) < 0.69 && one/(one+two) > 0.64)
  }
}

class GibbsTest extends FunSuite {
  test("Gibbs sampler on sim-3") {
    // Test that the objects gets made n stuff
    println("LOADING CORPUS...")
    println()
    val corpus = Io.rawCorpus(DataConsts.SIM_3_TRAIN_DOCS)
    
    println("BUILDING COLLAPSED SAMPLER")
    val cg = new CollapsedGibbs(corpus, 15, 0.1, 0.1, corpus.length)
    println("STATS:")
    println("\tVocabulary:\t" + cg.W)
    println("\tWords:\t\t" + cg.N)
    println("\tDocuments:\t" + cg.D)
    println()

    println("RUNNING EXPERIMENT")
    repeat(0, 3, cg)
    //println("w: \"" + cg.w.deep.mkString("\" \"") + "\"")
  }

  def repeat (i: Int, n: Int, cg: CollapsedGibbs): Unit = {
    if (i >= n) return
    else {
      cg.resampleTopic()
      println("ASSIGNMENT " + i + " COMPLETE")
      /*
       println("w: \"" + cg.w.deep.mkString("\" \"") + "\"")
       println("d: " + cg.d.deep.mkString(" "))
       println("z: " + cg.z.deep.mkString(" "))
       println("allAssignedZ: " + cg.allAssignedZ.deep.mkString(" "))
       println("wAssignedZ: " + cg.wAssignedZ.deep.mkString(" "))
       println("allAssignedZInD" + cg.allAssignedZInD.deep.mkString(" "))
       cg.resampleTopic()
       println("w: \"" + cg.w.deep.mkString("\" \"") + "\"")
       println("d: " + cg.d.deep.mkString(" "))
       println("z: " + cg.z.deep.mkString(" "))
       println("allAssignedZ: " + cg.allAssignedZ.deep.mkString(" "))
       println("wAssignedZ: " + cg.wAssignedZ.deep.mkString(" "))
       println("allAssignedZInD" + cg.allAssignedZInD.deep.mkString(" "))
       println()
       */
      repeat(i+1, n, cg)
    }
  }
}

