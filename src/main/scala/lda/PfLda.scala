package lda

import scala.collection.mutable.{ HashMap => HashMap }
import scala.util.{ Random => Random }

import globals.Constants
import stream._
import wrangle._

/** Particle filter-based Gibbs sampler for LDA.
 *
 * @param T Number of topics
 * @param alpha Symmetric Dirichlet prior
 * @param beta Symmetric Dirichlet prior
 * @param smplSize Number docs in reservoir sample
 * @param numParticles Number of particles to maintain
 * @param ess Controls threshold for rejuvenation. Higher = more often.
 */
class PfLda (val T: Int, val alpha: Double, val beta: Double,
             val smplSize: Int, val numParticles: Int, ess: Double,
             val rejuvBatchSize: Int, val rejuvMcmcSteps: Int) {
  val Whitelist = Text.stopWords(DataConsts.TNG_WHITELIST)
  var vocabToId = HashMap[String,Int]()
  var rejuvSeq = new ReservoirSampler[Array[String]](smplSize)
  var currVocabSize = 0
  var currWordIdx = 0
  
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
  def ingestDoc(doc: String): Int = {
    val Words = Text.bow(doc, (str: String) => Whitelist(str))

    val docId = newDocumentUpdate(Words) // happen before processing word!
    (0 to Words.length-1).foreach{ i => processWord(i, Words, docId) }
    docId
  }
  
  /** Process the ith entry in `words`; copied pretty much verbatim from
   Algorithm 4 of Canini, et al "Online Inference of Topics..." */
  private def processWord (i: Int, words: Array[String], docId: Int): Unit = {
    val currword = words(i)
    addWordIfNotSeen(currword) // side-effects; must be before particle updates!
    currWordIdx += 1

    particles.foreach { particle =>
      particle.unnormalizedReweight(currword, currVocabSize) }
    particles.foreach { particle =>
      particle.transition(i, words, currVocabSize,docId) }
    normalizeWeights()
    
    if (shouldRejuvenate()) rejuvenate()
  }

  /** Gets inverse 2-norm of particle weights, check against ESS */
  private def shouldRejuvenate (): Boolean = {
    val weights = particleWeightArray()
    val statistic = 1/math.pow(Math.norm(weights, 2), 2)
    statistic <= ess
  }

  private def newDocumentUpdate (doc: Array[String]): Int = {
    val index = rejuvSeq.addItem(doc)
    particles.foreach { particle => particle.newDocumentUpdate(index, doc) }
    index
  }
  
  private def rejuvenate (): Unit = {
    // resample the particles; 
    particles = multinomialResample()
    // pick rejuvenation sequence in the reservoir
    val wordIds = allWordIds()
    throw new RuntimeException("rejuvenate not implemented")
  }

  /** Array of wordIds; a word's id is a tuple (docId, wordIndex), where `docId`
   tells us where in `rejuvSeq` our document is, and `wordIndex`, which tells us
   where in that document our word is */
  private def allWordIds (): Array[(Int,Int)] = {
    val sample = rejuvSeq.getSampleSet
    val wordsInSample = sample.foldLeft(0){ (acc, doc) => acc + doc.length }
    var wordIds = new Array[(Int,Int)](wordsInSample)
    var currIdx = 0
    for (i <- 0 to sample.length-1) {
      for (j <- 0 to sample(i).length-1) {
        wordIds(currIdx) = (i,j); currIdx += 1
      }
    }

    // if our sample is bigger than the words seen
    if (currIdx > currWordIdx) {
      var smallerSet = new Array[(Int, Int)](currWordIdx)
      Array.copy(wordIds, 0, smallerSet, 0, currWordIdx)
      wordIds = smallerSet
    }
    wordIds
  }

  /** Creates an array of particles resampled proportional to the weights */
  private def multinomialResample (): Array[Particle] = {
    val weightsCdf = Stats.normalizeAndMakeCdf(particleWeightArray())
    val resampledParticles = new Array[Particle](numParticles)
    (0 to numParticles-1).foreach {
      i =>
        val indexOfParticleToCopy = Stats.sampleCategorical(weightsCdf)
      resampledParticles(i) = particles(indexOfParticleToCopy).copy
    }
    resampledParticles
  }

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

  def printParticles (): Unit = particles.foreach { p => println(p) }
}

class Particle (val topics: Int, val initialWeight: Double,
                val alpha: Double, val beta: Double,
                val rejuvSeq: ReservoirSampler[Array[String]]) {
  /* NOTE: `rejuvSeq` depends on the PfLda class to populate it with the
   documents that it will use for rejuvenation; it DEPENDS ON SIDE-EFFECTS to do
   its job. */
  var globalVect = new GlobalUpdateVector(topics)
  var weight = initialWeight
  var currDocVect = new DocumentUpdateVector(topics)
  var rejuvSeqAssignments = HashMap[Int,Array[Int]]()
  var rejuvSeqDocVects = HashMap[Int,DocumentUpdateVector]()

  /** Generates an unnormalized weight for the particle; returns new wgt. NOTE:
   side-effects on the particle's weight as well! */
  def unnormalizedReweight (word: String, w: Int): Double = {
    val prior = unnormalizedPrior(word, w)
    weight = weight * prior
    weight
  }

  /** "Transitions" particle to next state by sampling topic for `word`,
   which is our new observation. w is the *current* size of the vocabulary;
   returns that topic
   
   Behind the scenes, this requires two updates: first, we must update the
   global and document-specific update vectors, and then we must update the
   topic assignments if this document happens to be in our reservoir. */
  def transition (index: Int, words: Array[String], w: Int, docId: Int): Int = {
    val word = words(index)
    val cdf = updatePosterior(word, w)
    val sampledTopic = Stats.sampleCategorical(cdf)
    globalVect.update(word, sampledTopic)
    currDocVect.update(word, sampledTopic)

    if (docId != Constants.DidNotAddToSampler) {
      val currAssignments = rejuvSeqAssignments(docId)
      currAssignments(index) = sampledTopic
    }
    sampledTopic
  }

  def newDocumentUpdate (indexIntoSample: Int, doc: Array[String]): Unit = {
    currDocVect = new DocumentUpdateVector(topics)
    if (indexIntoSample != Constants.DidNotAddToSampler) {
      rejuvSeqAssignments(indexIntoSample) = new Array[Int](doc.length)
      rejuvSeqDocVects(indexIntoSample) = currDocVect
    }
  }
  
  /** Proper deep copy of the particle */
  def copy (): Particle = {
    val copiedParticle = new Particle(topics, initialWeight, alpha, beta,
                                      rejuvSeq)
    copiedParticle.globalVect = globalVect.copy
    copiedParticle.weight = weight
    copiedParticle.currDocVect = currDocVect.copy
    // copy rejuvSeqAssignments
    rejuvSeqAssignments.foreach
    { kv =>
      val copiedVal = new Array[Int](kv._2.length);
     copiedParticle.rejuvSeqAssignments(kv._1) = copiedVal;
     Array.copy(kv._2, 0, copiedVal, 0, kv._2.length) }
    // copy rejuvSeqDocVects
    rejuvSeqDocVects.foreach { kv =>
      copiedParticle.rejuvSeqDocVects(kv._1) = rejuvSeqDocVects(kv._1).copy() }
    copiedParticle
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
  private def updatePosterior (word: String, w: Int): Array[Double] = {
    var unnormalizedCdf = Array.fill(topics)(0.0)
    (0 to topics-1).foreach { i =>
      unnormalizedCdf(i) = updateEqn(word, i, w) }
    Stats.normalizeAndMakeCdf(unnormalizedCdf)
  }

  /** Applies the o-LDA update equation from "Online Inference of Topics..."
   by Canini, Shi, Griffiths. The relevant equation is eqn (2). w is the
   *current* size of the vocabulary */
  private def updateEqn (word: String, topic: Int, w: Int): Double = {
    val globalUpdate = (globalVect.numTimesWordAssignedTopic(word, topic)
                        + beta) /
    (globalVect.numTimesTopicAssignedTotal(topic) + w * beta)

    val docUpdate = (currDocVect.numTimesTopicOccursInDoc(topic) + alpha) /
    (currDocVect.numWordsInDoc + topics * alpha)
    globalUpdate * docUpdate
  }

  override def toString (): String = {
    var outstr = "Particle assignments:\n"
    rejuvSeqAssignments.foreach {
      kv => outstr += "\t" + kv._1 + " -> " + kv._2.deep.mkString(" ") + "\n"
    }
    outstr
  }
}

/** Tracks update progress for the document-specific ITERATIVE update
 equation of the particle filtered LDA implementation. */
class DocumentUpdateVector (val topics: Int) {
  // in the paper this is called n^{(d_j)}_{z_j, i\j}
  var timesTopicOccursInDoc = Array.fill[Int](topics)(0)
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

  /** proper deep copy of DocumentUpdateVector */
  def copy (): DocumentUpdateVector = {
    var copiedVect = new DocumentUpdateVector(topics)
    Array.copy(timesTopicOccursInDoc, 0, copiedVect.timesTopicOccursInDoc, 0,
               topics)
    copiedVect.wordsInDoc = wordsInDoc
    copiedVect
  }
}

/** Tracks update progress for the global iterative update equation of the
 particle filtered LDA implementation. */
class GlobalUpdateVector (val topics: Int) {
  // in the paper, this is called n^{(w_j)}_{z_j,i\j}
  var timesWordAssignedTopic = HashMap[(String,Int),Int]()
  // in the paper, this is called n^{(.)}_{z_j, i\j}
  var timesTopicAssignedTotal = Array.fill[Int](topics)(0)

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

  /** proper deep copy of DocumentUpdateVector */
  def copy (): GlobalUpdateVector = {
    val copiedVect = new GlobalUpdateVector(topics)
    timesWordAssignedTopic.foreach { kv =>
      copiedVect.timesWordAssignedTopic(kv._1) = kv._2 }
    Array.copy(timesTopicAssignedTotal, 0, copiedVect.timesTopicAssignedTotal, 0,
               topics)
    copiedVect
  }
}
