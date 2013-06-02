package tests

import lda._
import stream._
import wrangle._

import org.scalatest.FunSuite
import java.util.{ Arrays => Arrays }
import scala.collection.{ Set => Set }
import scala.util.{ Random => Random }


class ParticleTests extends FunSuite {
  /** numdocs is the number of documents to store in the reservoir sampler */
  def buildParticleStore (numDocs: Int): ParticleStore = {
    val t = 2
    val alpha = 0.1
    val beta = 0.1
    val numParticles = 5
    val ess = 20
    val rejuvBatchSize = 256
    val rejuvSeq = new ReservoirSampler[Array[String]](numDocs)
    var ps = new ParticleStore (t, alpha, beta, numParticles, ess,
                                rejuvBatchSize, rejuvSeq)
    ps
  }

  test("AssignmentStore.getTopic") {
    var as = new AssignmentStore()
    as.newParticle(0, -1)
    as.newDocument(0, 0)
    val thrown = intercept[java.util.NoSuchElementException] {
      as.getTopic(0,0,0)
    }
    assert(true)
    // TODO: add more to this to make sure that as topic assignments are added,
    // this keeps being correct
  }

  test("AssignmentStore.setTopic") {
    var as = new AssignmentStore()
    val particleId = 0
    val docId = 0
    as.newParticle(0, -1)
    as.newDocument(0, 0)
  }
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

