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
  var rejuvSeq = new ReservoirSampler[Array[String]](smplSize)
  var currVocabSize = 0
  
  var particles = Array.fill(numParticles)(new Particle(T, 1.0/numParticles,
							alpha, beta, rejuvSeq))

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
    val Words = Text.bow(doc, (str: String) => Whitelist(str))

    addToReservoir(Words) // side-effects; must happen before processing word!
    (0 to Words.length-1).foreach{ i => processWord(i, Words) }
  }
  
  /** Process the ith entry in `words`; copied pretty much verbatim from
   Algorithm 4 of Canini, et al "Online Inference of Topics..." */
  private def processWord (i: Int, words: Array[String]): Unit = {
    val currword = words(i)
    addWordIfNotSeen(currword) // side-effects; must be before particle updates!
    
    particles.foreach { particle =>
      particle.unnormalizedReweight(currword, currVocabSize) }
    particles.foreach { particle => particle.transition(currword,
							currVocabSize) }
    normalizeWeights()
    
    if (shouldRejuvenate()) rejuvenate()
  }

  /** Gets inverse 2-norm of particle weights, check against ESS */
  private def shouldRejuvenate (): Boolean = {
    val weights = particleWeightArray()
    Math.norm(weights, 2) <= ess
  }

  private def addToReservoir (doc: Array[String]): Unit = {
    rejuvSeq.add(doc)
  }
  
  private def rejuvenate (): Unit =
    throw new RuntimeException("rejuvenate not implemented")

  /** Adds `word` to the current vocab map if not seen; uses current
   currVocabSize as the id, i.e., if `word` is the nth seen so far, then n
   happens to be == currVocabSize
   */
  private def addWordIfNotSeen (word: String): Unit = {
    if (!(vocabToId contains word)) {
      vocabToId(word) = currVocabSize
      currVocabSize += 1
    }
  }

  /** Takes weights of particles, normalizes them, writes them back; note:
   SIDE-EFFECTS. */
  private def normalizeWeights (): Unit = {
    var weights = particleWeightArray()
    Stats.normalize(weights)
    for (i <- 0 to particles.length-1) particles(i).weight = weights(i)
  }

  /** Helper method puts the weights of particles into an array, so that
   `particles(i) == weights(i)` */
  private def particleWeightArray (): Array[Double] = {
    var weights = Array.fill(particles.length)(0.0)
    for (i <- 0 to particles.length-1) weights(i) = particles(i).weight
    weights
  }
}

class Particle (val topics: Int, val initialWeight: Double,
		val alpha: Double, val beta: Double,
		val rejuvSeq: ReservoirSampler[Array[String]]) {
  /* NOTE: `rejuvSeq` depends on the PfLda class to populate it with the
   documents that it will use for rejuvenation; it DEPENDS ON SIDE-EFFECTS to do
   its job. */
  var docVect = new DocumentUpdateVector(topics)
  var globalVect = new GlobalUpdateVector(topics)
  var weight = initialWeight

  /** Generates an unnormalized weight for the particle; returns new wgt. NOTE:
   side-effects on the particle's weight as well! */
  def unnormalizedReweight (word: String, w: Int): Double = {
    val prior = unnormalizedPrior(word, w)
    weight = weight * prior
    weight
  }

  /** "Transitions" particle to next state by sampling topic for `word`,
   which is our new observation. w is the *current* size of the vocabulary;
   returns that topic */
  def transition (word: String, w: Int): Int = {
    val cdf = posterior(word, w)
    val sampledTopic = Stats.sampleCategorical(cdf)
    docVect.update(word, sampledTopic)
    globalVect.update(word, sampledTopic)
    sampledTopic
  }

  /** Results in a number proportional to P(w_i|z_{i-1}, w_{i-1}); specifically,
   we note that this probability is proportional to P(w_i|z_{i-1}^{(p)})
   P(z_{i-1}^{(p)}|d_i). */
  private def unnormalizedPrior (word: String, w: Int): Double = {
    var prOfWord = 0.0
    (0 to topics-1).foreach { t => prOfWord += updateEqn(word, t, w) }
    prOfWord
  }

  /** Generates the normalized posterior distribution P(z_j|Z_{i-1}, w_i);
   w is the *current* size of the vocabulary */
  private def posterior (word: String, w: Int): Array[Double] = {
    var unnormalizedCdf = Array.fill(topics)(0.0)
    (0 to topics-1).foreach { i =>
      unnormalizedCdf(i) = updateEqn(word, i, w) }
    Stats.normalize(unnormalizedCdf)
  }

  /** Applies the o-LDA update equation from "Online Inference of Topics..."
   by Canini, Shi, Griffiths. The relevant equation is eqn (2). w is the
   *current* size of the vocabulary */
  private def updateEqn (word: String, topic: Int, w: Int): Double = {
    val globalUpdate = (globalVect.numTimesWordAssignedTopic(word, topic)
			+ beta) /
    (globalVect.numTimesTopicAssignedTotal(topic) + w * beta)

    val docUpdate = (docVect.numTimesTopicOccursInDoc(topic) + alpha) /
    (docVect.numWordsInDoc + topics * alpha)
    globalUpdate * docUpdate
  }
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
    if (timesWordAssignedTopic contains (word,topic))
      timesWordAssignedTopic((word, topic))
    else 0

  def numTimesTopicAssignedTotal (topic: Int): Int =
    timesTopicAssignedTotal(topic)

  /** Updates vector based on observation: word and topic assigned to it */
  def update (word: String, topic: Int): Unit = {
    if (timesWordAssignedTopic contains (word,topic))
      timesWordAssignedTopic((word, topic)) += 1
    else timesWordAssignedTopic((word,topic)) = 1
    timesTopicAssignedTotal(topic) += 1
  }
}
