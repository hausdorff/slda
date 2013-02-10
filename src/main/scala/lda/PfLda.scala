package lda

import scala.collection.mutable.{ HashMap => HashMap }
import scala.util.{ Random => Random }

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
	     val smplSize: Int, val numParticles: Int, ess: Double) {
  val Whitelist = Text.stopWords(DataConsts.TNG_WHITELIST)
  var vocabToId = HashMap[String,Int]()
  var cntWrdAssgTopicInCrps = HashMap[(String,Int),Int]()
  var cntTopicAssgInCrps = Array.fill(T)(0)
  var pweights = Array.fill(numParticles)(1.0/numParticles)
  var particles = Array.fill(numParticles)(new Random().nextInt(T))
  var vocabSize = 0

  /** Ingests set of documents, updating LDA run as we go */
  def ingestDocs (docs: Array[String]): Unit =
    docs.foreach{ doc => ingestDoc(doc) }
  
  /** Ingest one document, update LDA as we go */
  def ingestDoc(doc: String): Unit = {
    // get all words in doc (remember to strip out whitelist
    val Words = Text.bow(doc, (str: String) => Whitelist(str))
    var topicDistr = Array.fill(T)(0.0)
    Words.foreach{ word => processWord(word, topicDistr) }
  }
  
  private def processWord (word: String, topicDistr: Array[Double]): Unit = {
    (0 to pweights.length-1).foreach { i => reweightParticle(i) }
    // sample topic assignment for current word for each particle
    (0 to particles.length-1).foreach { i => resampleParticle(i) }
    // normalize weights
    Stats.normalize(pweights)
    // get inverse 2-norm of weights, check against ESS
    if (Math.norm(pweights, 2) <= ess) rejuvenate()
  }

  private def rejuvenate (): Unit = { }

  private def reweightParticle (i: Int): Unit = {
    // \omega_i^{(p)} = \omega_{i-1}^{(p)} P(w_i | z_{i-1}^{(p)}, w_{i-1})
  }

  private def resampleParticle (i: Int): Unit = { }
}

