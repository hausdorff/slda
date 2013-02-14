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
  var particles = Array.fill(numParticles)(new Particle(T, 1.0/numParticles))
  var vocabSize = 0

  /** Ingests set of documents, updating LDA run as we go */
  def ingestDocs (docs: Array[String]): Unit =
    docs.foreach{ doc => ingestDoc(doc) }
  
  /** Ingest one document, update LDA as we go
   *
   * For each new word, we reweight the particles. Then we sample a topic
   * assignment from the posterior. Then if the 2norm of the weight vector
   * lies below a certain threshold, we resample the topics
   */
  def ingestDoc(doc: String): Unit = {
    // get all words in doc (remember to strip out whitelist
    val Words = Text.bow(doc, (str: String) => Whitelist(str))

    (0 to Words.length-1).foreach{ i => processWord(i, Words) }
    //addWordsToReservoir()
  }
  
  /** Process the ith entry in `words` */
  private def processWord (i: Int, words: Array[String]): Unit = {
    if (i == 0) {
      throw new RuntimeException("processWord is unable to proc the first word right now!")
      //initializeParticle()
    }
    
    val currword = words(i)
    // side-effects; must happen before particle updates!
    addWordIfNotSeen(currword)
    
    particles.foreach { particle => particle.unnormalizedReweight(currword) }
    particles.foreach { particle => particle.transition(currword,
							currVocabSize) }
    normalizeWeights()
    // get inverse 2-norm of weights, check against ESS
    //if (Math.norm(pweights, 2) <= ess) rejuvenate()
  }

  private def rejuvenate (): Unit = { throw new RuntimeException("rejuvenate not implemented") }

  /** Adds `word` to the current vocab map; uses current vocabSize as the id,
   i.e., if `word` is the nth seen so far, then n happens to be == vocabSize
   */
  private def addWordIfNotSeen (word: String): Unit = {
    if (!(vocabToId contains word)) {
      vocabToId(word) = vocabSize
      vocabSize += 1
    }
  }

  /** Takes weights of particles, normalizes them, writes them back; note:
   SIDE-EFFECTS. */
  private def normalizeWeights (): Unit = {
    var weights = Array.fill(particles.length)(0.0)
    for (i <- 0 to particles.length-1) weights(i) = particles(i).weight
    Stats.normalize(weights)
    for (i <- 0 to particles.length-1) particles(i).weight = weights(i)
  }
}

class Particle (val topics: Int, val initialWeight: Double) {
  var docVect = new DocumentUpdateVector(topics)
  var globalVect = new GlobalUpdateVector(topics)
  var weight = initialWeight

  /** Generates an unnormalized weight for the particle; returns new wgt */
  def unnormalizedReweight (word: String): Double =
    // \omega_i^{(p)} = \omega_{i-1}^{(p)} P(w_i | z_{i-1}^{(p)}, w_{i-1})
    throw new RuntimeException("unnormalizedReweight not implemented")

  /** "Transitions" particle to next state by sampling topic for `word`,
   which is our new observation; returns that topic */
  def transition (word: String): Int = {
    val cdf = posterior(word)
    val sampledTopic = Stats.sampleCategorical(cdf)
    docVect.update(word, sampledTopic)
    globalVect.update(word, sampledTopic)
    sampledTopic
  }

  /** Generates the posterior distribution P(z_j|Z_{i-1}, w_i) */
  private def posterior (word: String): Array[Double] =
    throw new RuntimeException("posterior not implemented")
}

/** Tracks update progress for the document-specific iterative update
 equation of the particle filtered LDA implementation. */
class DocumentUpdateVector (val topics: Int) {
  // in the paper this is called n^{(d_j)}_{z_j, i\j}
  var timesTopicOccursInDoc = Array.fill(topics)(0)
  // in the paper this is called n^{(d_j)}_{., i\j}
  var wordsInDoc = 0

  def numTimesTopicOccursInDoc (topic: Int): Int =
    timesTopicOccursInDoc(topic)

  def numWordsInDoc (): Int = wordsInDoc

  /** Update vector based on observation: word and topic assigned to it */
  def update (word: String, topic: Int): Unit = {
    timesTopicOccursInDoc(topic) += 1
    wordsInDoc += 1
    throw new RuntimeException("not sure if wordsInDoc should be updated...")
  }
}

/** Tracks update progress for the global iterative update equation of the
 particle filtered LDA implementation. */
class GlobalUpdateVector (val topics: Int) {
  // in the paper, this is called n^{(w_j)}_{z_j,i\j}
  var timesWordAssignedTopic = HashMap[(String,Int),Int]()
  // in the paper, this is called n^{(.)}_{z_j, i\j}
  var timesTopicAssignedTotal = Array.fill(topics)(0)

  def numTimesWordAssignedTopic (word: String, topic: Int): Int =
    timesWordAssignedTopic((word, topic))

  def numTimesTopicAssignedTotal (topic: Int): Int =
    timesTopicAssignedTotal(topic)

  /** Updates vector based on observation: word and topic assigned to it */
  def update (word: String, topic: Int): Unit = {
    timesWordAssignedTopic((word, topic)) += 1
    timesTopicAssignedTotal(topic) += 1
  }
}
