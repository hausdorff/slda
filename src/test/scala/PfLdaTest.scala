package tests

import lda._
import wrangle._

import org.scalatest.FunSuite
import java.util.{ Arrays => Arrays }
import scala.collection.{ Set => Set }


class PfLda extends FunSuite {
  test("Test ingestDoc for particle filter-based LDA") {
    val pflda = new lda.PfLda(2, 0.1, 0.1, 2, 20, 0.2)
    val corpus = Io.rawCorpus(DataConsts.SIM_3_TRAIN_DOCS)
    pflda.ingestDocs(corpus)

    //println(pflda.Whitelist)
    println(pflda.vocabToId.size)
    println("weights: " + pflda.pweights.deep)
    println("cntWrdAssgTopicInCrps: " + pflda.cntWrdAssgTopicInCrps)
    println("cntTopicAssgInCrps: " + pflda.cntTopicAssgInCrps.deep)
    println(pflda.particles.deep)
  }
}
