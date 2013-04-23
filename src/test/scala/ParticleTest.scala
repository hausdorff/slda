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

  /** Persists documents as we would expect */
  test("particle document collection persisted correctly") {
    var ps = buildParticleStore(10)
  }

  /*
  test("test copy mechanism") {
    var srcParticle = new Particle(3, 1.0/3, 0.1, 0.1,
                                   new ReservoirSampler(3))
    val newItemIdx = srcParticle.rejuvSeq.add(Array("cows", "are",
                                                    "delicious"))
    var dstParticle = srcParticle.copy()
    // make sure the rejuvseq gets pointed at the same place
    assert(srcParticle.rejuvSeq == dstParticle.rejuvSeq)
    assert(srcParticle.rejuvSeq.getSampleSet.deep ==
      dstParticle.rejuvSeq.getSampleSet.deep)

    // make sure update vectors are pointed at *different* places, but
    // still currently have the same values
    assert(srcParticle.globalVect != dstParticle.globalVect)
    assert(srcParticle.currDocVect != dstParticle.currDocVect)
    // tricky; collections override `==`
    assert(srcParticle.rejuvSeqAssignments ==
      dstParticle.rejuvSeqAssignments)
    srcParticle.rejuvSeqAssignments(3) = Array[Int](1,2,3)
    assert(srcParticle.rejuvSeqAssignments !=
      dstParticle.rejuvSeqAssignments)
    
    assert(srcParticle.rejuvSeqDocVects ==
      dstParticle.rejuvSeqDocVects)
    srcParticle.rejuvSeqDocVects(4) = new DocumentUpdateVector(3)
    assert(srcParticle.rejuvSeqDocVects !=
      dstParticle.rejuvSeqDocVects)
  }
  */
}

