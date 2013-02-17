package tests

import lda._
import wrangle._

import org.scalatest.FunSuite
import java.util.{ Arrays => Arrays }
import scala.collection.{ Set => Set }


class PfLda extends FunSuite {
  test("Test that PfLda is constructed correctly") {
    val topics = 3
    val alpha = 0.1
    val beta = 0.1
    val sampleSize = 5
    val numParticles = 5
    val ess = 1
    var pflda = new lda.PfLda(topics, alpha, beta, sampleSize, numParticles,
			      ess)
    val corpus = Array("the cow is short", "I can't find my glasses",
		       "walnuts are delicious")
    pflda.ingestDocs(corpus)
    
    // Test globals roughly look ok
    assert(pflda.currVocabSize == 5)
    assert(pflda.rejuvSeq.size == 3)
    assert(pflda.particles.size == 5)

    var targetWeights = Array.fill(numParticles)(0.0)
    (0 to numParticles-1).foreach { i =>
      targetWeights(i) = pflda.particles(i).weight }
    assert(math.abs(targetWeights.reduceLeft(_+_)-1) <= 0.1)
  }
  
  /*
  test("Test ingestDoc for particle filter-based LDA") {
    val pflda = new lda.PfLda(2, 0.1, 0.1, 2, 20, 0.2)
    val corpus = Io.rawCorpus(DataConsts.SIM_3_TRAIN_DOCS)
    pflda.ingestDocs(corpus)

    //println(pflda.Whitelist)
    println(pflda.vocabToId.size)
    println("weights: " + pflda.particles.deep)
  }
  */
}
