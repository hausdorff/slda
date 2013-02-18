package tests

import lda._
import wrangle._

import org.scalatest.FunSuite
import java.util.{ Arrays => Arrays }
import scala.collection.{ Set => Set }
import scala.util.{ Random => Random }


class PfLdaTests extends FunSuite {
  val r = new Random()
  def generateWord (vocabulary: Array[String], topic: Array[Double]) = {
    val wordIdx = Stats.sampleCategorical(topic)
    vocabulary(wordIdx)
  }
  
  def generateDoc (vocabulary: Array[String], doclength: Int,
		   topic1: Array[Double], topic2: Array[Double],
		   mixture: Array[Double]): Array[String] = {
    var doc = new Array[String](doclength)
    (0 to doclength-1).foreach
    { i => val currTopic = Stats.sampleCategorical(mixture)
     if (currTopic == 0) doc(i) = generateWord(vocabulary, topic1)
     else doc(i) = generateWord(vocabulary, topic2) }
    doc
  }

  /** results in array of tuples containing the mixture (element 1) and the
   actual document (element 2) */
  def generateCorpus (): Array[(Array[Double], Array[String])] = {
    //(0 to 10).foreach{ i => println(r.nextInt(4)) }
    //(0 to 10).foreach{ i => println(r.nextDouble()) }
    val documents = 16
    val doclength = 16
    val vocabulary = Array[String]("river", "stream", "bank", "money", "loan")
    // Money topic
    val topic1 = Array[Double](0, 0, 1.0/3, 1.0/3, 1.0/3)
    val topic1Cdf = Stats.normalizeAndMakeCdf(topic1)
    // Nature topic
    val topic2 = Array[Double](1.0/3, 1.0/3, 1.0/3, 0, 0, 0)
    val topic2Cdf = Stats.normalizeAndMakeCdf(topic2)
    
    var corpus = new Array[(Array[Double], Array[String])](documents)
    (0 to documents-1).foreach
    { i => val mixture = Array[Double](r.nextDouble(), 1)
     corpus(i) = (mixture,
		  generateDoc(vocabulary, doclength, topic1, topic2,
			      mixture)) }
    
    corpus
  }
  
  test("build test corpus") {
    val corpus = generateCorpus()
    corpus.foreach{ item => println((item._1.deep.mkString(" "),
				     item._2.deep.mkString(" "))) }
  }
  
  /*
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
    assert(pflda.rejuvSeq.occupied == 3)
    assert(pflda.particles.size == numParticles)

    var targetWeights = Array.fill(numParticles)(0.0)
    (0 to numParticles-1).foreach { i =>
      targetWeights(i) = pflda.particles(i).weight }
    assert(math.abs(targetWeights.reduceLeft(_+_)-1) <= 0.1)
  }
  */
  
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
