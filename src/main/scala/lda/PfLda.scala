package lda

import scala.collection.mutable.{ HashMap => HashMap }

import stream._
import wrangle._

/** Particle filter-based Gibbs sampler for LDA.
 *
 * @param T Number of topics
 * @param alpha Symmetric Dirichlet prior
 * @param beta Symmetric Dirichlet prior
 * @param smplSize Number docs in reservoir sample
 * @param rejuvSteps Number of steps to take before rejuvenating
 */
class PfLda (val T: Int, val alpha: Double, val beta: Double,
	     val smplSize: Int, val rejuvSteps: Int) {
  val Whitelist = Text.stopWords(DataConsts.TNG_WHITELIST)
  var vocabToId = HashMap[String,Int]()
  var cntWrdAssgTopicInCrps = HashMap[(String,Int),Int]()
  var cntTopicAssgInCrps = Array(T)(0)
  var currWord = 0

  def ingestDocs (docs: Array[String]): Unit =
    for (i <- 0 to docs.length-1) ingestDoc(docs(i))
  
  /*
   we need:
   # of times word w_j is assigned to topic z_j
   # of times any word is assigned to topic z_j
   # of times a word in doc d_i is assigned to topic z_j
   # of wordsin document d_i
   */
  def ingestDoc(doc: String): Unit = {
    // get all words in doc (remember to strip out whitelist
    val Words = Text.bow(doc, (str: String) => Whitelist(str))
    
    for (i <- 0 to Words.length-1) {
      val w = Words(i)
      // injest any new words into the word map
      if (!(vocabToId contains Words(i))) {
	vocabToId(w) = currWord
	currWord += 1
      }
      // loop: every word, build and normalize the update distribution,
      // sample, and place sampled assignment on word; then go on to the
      // next word
      

      // add document to reservoir sample
    }
  }

  def unnormalizedPtUpdate (): Double = {
    0.0
  }
}

