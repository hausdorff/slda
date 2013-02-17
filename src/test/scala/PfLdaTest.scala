package tests

import lda._
import wrangle._

import org.scalatest.FunSuite
import java.util.{ Arrays => Arrays }
import scala.collection.{ Set => Set }


class PfLda extends FunSuite {
  test("Test that PfLda is constructed correctly") {
    var pflda = new lda.PfLda(3, 0.1, 0.1, 5, 5, 1)
    val corpus = Array("the cow is short", "I can't find my glasses",
		       "walnuts are delicious")
    pflda.ingestDocs(corpus)
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
