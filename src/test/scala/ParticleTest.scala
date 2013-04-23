package tests

import lda._
import stream._
import wrangle._

import org.scalatest.FunSuite
import java.util.{ Arrays => Arrays }
import scala.collection.{ Set => Set }
import scala.util.{ Random => Random }


class ParticleTests extends FunSuite {
  /** Persists documents as we would expect */
  test("particle document collection persisted correctly") {
    val t = 3
    val alpha = 0.1
    val beta = 0.1
    val numParticles = 3
    val ess = 0.1
    val rejuvBatchSize = 10
    val numdocs = 10
    val rejuvSeq = new ReservoirSampler[Array[String]](numdocs)
    var ps = new ParticleStore (3, 0.1, 0.1, numParticles, ess, rejuvBatchSize,
                                rejuvSeq)
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

