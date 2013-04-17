package tests

import lda._
import stream._
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
                   mixture: Array[Double]): String = {
    var doc = new Array[String](doclength)
    (0 to doclength-1).foreach
    { i => val currTopic = Stats.sampleCategorical(mixture)
     if (currTopic == 0) {
       doc(i) = generateWord(vocabulary, topic1)
     }
     else {
       doc(i) = generateWord(vocabulary, topic2)
     }
   }
    doc.deep.mkString(" ")
  }

  /** results in array of tuples containing the mixture (element 1) and the
   actual document (element 2) */
  def generateCorpus (): Array[(Array[Double], String)] = {
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
    
    var corpus = new Array[(Array[Double], String)](documents)
    (0 to documents-1).foreach
    { i => val mixture = Array[Double](r.nextDouble(), 1)
     corpus(i) = (mixture,
                  generateDoc(vocabulary, doclength, topic1, topic2,
                              mixture)) }
    
    corpus
  }

  def standardPfLda (): PfLda = {
    val topics = 2
    val alpha = 0.1
    val beta = 0.1
    val sampleSize = 16
    val numParticles = 5
    val ess = 20
    val rejuvBatchSize = 128
    val rejuvMcmcSteps = 32
    var pflda = new lda.PfLda(topics, alpha, beta, sampleSize,
                              numParticles, ess, rejuvBatchSize,
                              rejuvMcmcSteps)
    pflda
  }

  test("build test corpus") {
    val corpus = generateCorpus()
    /*
     corpus.foreach{ item => println((item._1.deep.mkString(" "),
     item._2.deep.mkString(" "))) } */
    val pflda = standardPfLda()
    //pflda.ingestDoc(corpus(0)._2)
    //pflda.ingestDoc(corpus(1)._2)

    //println(pflda.vocabToId.size)
    //println(pflda.currVocabSize)
    //println(pflda.rejuvSeq)
    // ingest documents, store a map of where they are in Reservoir Sampler
    var map = new Array[Int](corpus.length)
    for (i <- 0 to corpus.length-1) {
      print(i + " ")
      map(i) = pflda.ingestDoc(corpus(i)._2)
    }

    // iterate through documents, output accuracy averaged over all particles
    for (i <- 0 to map.length-1) {
      var sum:Double = 0
      val mappedIdx = map(i)
      println("(money, nature)")
      println(corpus(i)._1.deep)
      println(corpus(i)._2)
      pflda.particles.particles foreach {
        p =>
          println(p.rejuvSeqAssignments(mappedIdx).deep)
          val s = p.rejuvSeqAssignments(mappedIdx).reduceLeft(_+_)
        sum += s / 16.0
        //println(p.rejuvSeqAssignments(mappedIdx).deep)
      }
      println((1 - (sum / pflda.particles.particles.length.toDouble)) + " " +
              (sum / pflda.particles.particles.length.toDouble))
      println("note that topic labels are determined by coinflip, so '1' " +
            " doesn't necessarily correspond to topic #1")
      println()
    }
    
    //pflda.printParticles
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

class GlobalUpdateVectorTests extends FunSuite {
  test("test copy mechanism") {
    // Tests that if we copy one vector, mutating one won't mutate the other
    val srcVect = new GlobalUpdateVector(3)
    srcVect.timesWordAssignedTopic(("cows", 3)) = 10
    srcVect.timesWordAssignedTopic(("are", 2)) = 11
    srcVect.timesWordAssignedTopic(("delicious", 1)) = 9
    srcVect.timesTopicAssignedTotal(0) = 1
    srcVect.timesTopicAssignedTotal(1) = 1
    srcVect.timesTopicAssignedTotal(2) = 1
    val dstVect = srcVect.copy()
    
    dstVect.timesTopicAssignedTotal(0) = 17
    dstVect.timesWordAssignedTopic(("cows", 3)) = 101
    
    assert(srcVect.timesTopicAssignedTotal(0) !=
      dstVect.timesTopicAssignedTotal(0))
    assert(srcVect.timesWordAssignedTopic(("cows", 3)) !=
      dstVect.timesWordAssignedTopic(("cows", 3)))

    srcVect.timesTopicAssignedTotal(0) = 180
    srcVect.timesWordAssignedTopic(("cows", 3)) = 2
    
    assert(srcVect.timesTopicAssignedTotal(0) !=
      dstVect.timesTopicAssignedTotal(0))
    assert(srcVect.timesWordAssignedTopic(("cows", 3)) !=
      dstVect.timesWordAssignedTopic(("cows", 3)))
  }
}

class DocumentUpdateVectorTests extends FunSuite {
  test("test copy mechanism") {
    // Tests that if we copy one vector, mutating one won't mutate the other
    val srcVect = new DocumentUpdateVector(3)
    srcVect.timesTopicOccursInDoc(0) = 1
    srcVect.timesTopicOccursInDoc(1) = 1
    srcVect.timesTopicOccursInDoc(2) = 1
    val dstVect = srcVect.copy()
    
    dstVect.timesTopicOccursInDoc(0) = 17
    dstVect.wordsInDoc = 20

    assert(srcVect.timesTopicOccursInDoc(0) !=
      dstVect.timesTopicOccursInDoc(0))
    assert(srcVect.wordsInDoc != dstVect.wordsInDoc)

    srcVect.timesTopicOccursInDoc(0) = 180
    srcVect.wordsInDoc = 7
    
    assert(srcVect.timesTopicOccursInDoc(0) !=
      dstVect.timesTopicOccursInDoc(0))
    assert(srcVect.wordsInDoc != dstVect.wordsInDoc)
  }
}
