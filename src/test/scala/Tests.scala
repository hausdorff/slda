package tests

import gibbs._
import lda.Stats
import lda.Math
import wrangle._
import org.scalatest.FunSuite
import java.util.{ Arrays => Arrays }

object Helpers {
  // terribly abstracted; counts ratio of first element to second over many
  // experiments
  def samplingExpt(cdf: Array[Double]): Double = {
    var one = 0
    var two = 0.0
    for (i <- 0 to 10000000) {
      val res = Stats.sampleCategorical(cdf)
      if (res == 0) one += 1
      else two += 1
    }
    one/(one+two)
  }
}

class GibbsUtilTests extends FunSuite {
  test("copy1dArr") {
    val a1 = Array(1, 2, 3)
    var res1 = GibbsUtil.copy1dArr[Int](a1)
    val target1 = Array(1, 2, 3)
    
    assert(res1.deep == target1.deep)
    res1(0) = 2
    // make sure the deep references point to different arrays
    assert(res1.deep != target1.deep)
    
    assert(Arrays.equals(GibbsUtil.copy1dArr[Int](Array()), Array[Int]()))
  }
  
  test("copy2dArr") {
    val a1 = Array(Array(1), Array(2))
    var res1 = GibbsUtil.copy2dArr[Int](a1)
    val target1 = Array(Array(1), Array(2))
    
    assert(res1.deep == target1.deep)
    res1(0)(0) = 2
    // make sure the deep references point to different arrays
    assert(res1.deep != target1.deep)
    
    assert(GibbsUtil.copy2dArr[Int](Array()).deep == Array[Int]().deep)
    assert(GibbsUtil.copy2dArr[Int](Array(Array())).deep ==
      Array(Array()).deep)
  }
}

class StatsTests extends FunSuite {
  test("sampleWithoutReplacement") {
    val arr1 = Array(0,1,2,3,4,5,6,7,8,9)
    val outcomes = new Array[Int](10)
    (0 to 100000).foreach{
      i => val sample = Stats.sampleWithoutReplacement(arr1, 2)
      outcomes(sample(0)) += 1
      outcomes(sample(1)) += 1
    }
    outcomes.foreach { o => assert(o > 19000 && o < 21000,
				   "sampleWithoutReplacement failed " +
				   "statistical test") }
  }
  
  test("normalizeAndMakeCdf") {
    val arr0 = Array[Double]()
    val arr1 = Array[Double](3)
    val arr2 = Array[Double](0.2)
    val arr3 = Array[Double](1, 1, 1, 1)
    val arr4 = Array[Double](1, 1, 1)
    val arr5 = Array[Double](1, 1)
    val arr6 = Array[Double](2, 1)
    
    assert(Arrays.equals(Stats.normalizeAndMakeCdf(arr0), Array[Double]()),
	   "array should be empty")
    assert(Arrays.equals(Stats.normalizeAndMakeCdf(arr1), Array[Double](1)),
	   "array should be unit array")
    assert(Arrays.equals(Stats.normalizeAndMakeCdf(arr2), Array[Double](1)),
	   "array should be unit array")
    assert(Arrays.equals(Stats.normalizeAndMakeCdf(arr3),
			 Array(0.25, 0.5, 0.75, 1.0)),
	 "elements are equiprobable")
    assert(Arrays.equals(Stats.normalizeAndMakeCdf(arr4),
			 Array(1.0/3.0, 2.0/3.0, 1)),
	   "elements are equiprobable")
    assert(Arrays.equals(Stats.normalizeAndMakeCdf(arr5), Array(0.5, 1)),
	   "elements are equiprobable")
    assert(Arrays.equals(Stats.normalizeAndMakeCdf(arr6), Array(2.0/3.0, 1)))
  }

  test("sampleCategorical") {
    assert(Stats.sampleCategorical(Stats.normalizeAndMakeCdf(Array(1))) == 0)
    
    val arr1 = Array[Double](2, 1)
    val cdf1 = Stats.normalizeAndMakeCdf(arr1)
    val res1 = Helpers.samplingExpt(cdf1)
    assert(res1 < 0.69 && res1 > 0.64,
	   "statistical test failed with value " + res1 +
	   "should have been <0.69 and > 0.64; this could either be " +
	   "chance or it could be a real issue.")

    val arr2 = Array[Double](1,2,1)
    val cdf2 = Stats.normalizeAndMakeCdf(arr2)
    val res2 = Helpers.samplingExpt(cdf2)
    assert(res2 < 0.28 && res2 > 0.22,
	   "statistical test failed with value " + res2 +
	   "should have been <0.22 and > 0.28; this could either be " +
	   "chance or it could be a real issue.")
  }
}

class MathTests extends FunSuite {
  test("norm") {
    val a1 = Array(1.0,2,3,4)
    val answer1 = Math.norm(a1, 1)
    val answer2 = Math.norm(a1, 2)
    val target1 = 10
    val target2 = math.pow(30, 0.5)
    assert(answer1 == target1, "norms don't match: " + target1 + " " +
	   answer1)
    assert(answer2 == target2, "norms don't match: " + target2 + " " +
	   answer2)
  }
  
  test("max tests") {
    assert(Math.max(0,0) == 0)
    assert(Math.max(0,1) == 1)
    assert(Math.max(1,0) == 1)
    assert(Math.max(-1,0) == 0)
    
    assert(Math.max(0.0,0) == 0.0)
    assert(Math.max(0.0,1) == 1)
    assert(Math.max(0,1.0) == 1)
    assert(Math.max(-1.0,0.0) == 0.0)
  }

  test("min tests") {
    assert(Math.min(0,0) == 0)
    assert(Math.min(0,1) == 0)
    assert(Math.min(1,0) == 0)
    assert(Math.min(-1,0) == -1)
    
    assert(Math.min(0.0,0) == 0.0)
    assert(Math.min(0.0,1) == 0)
    assert(Math.min(0,1.0) == 0)
    assert(Math.min(-1.0,0.0) == -1.0)
  }
}

class GibbsTest extends FunSuite {
  test("Load all data") {
    val sim3 = Io.rawCorpus(DataConsts.SIM_3_TRAIN_DOCS)
    val rel3 = Io.rawCorpus(DataConsts.REL_3_TRAIN_DOCS)
    val diff3 = Io.rawCorpus(DataConsts.DIFF_3_TRAIN_DOCS)
    println(sim3.length + " " + rel3.length + " " + diff3.length)
  }
  
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

