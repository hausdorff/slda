package lda

import scala.collection.mutable.{ HashMap => HashMap }

import globals.Constants
import stream._

/** A memory- and time-efficient way to represent particles, as detailed in
 section 4 of Canini Shi Griffiths */
class ParticleStore (val T: Int, val alpha: Double, val beta: Double,
                     val numParticles: Int,
                     var rejuvSeq: ReservoirSampler[Array[String]]) {
  /* (1) generate particles, eg,
   val p = new Particle(T, 1.0/numParticles, alpha, beta, rejuvSeq) */
  var particles = Array.fill(numParticles)(new Particle(T, 1.0/numParticles,
                                                        alpha, beta, rejuvSeq))

  def unnormalizedReweightAll (currword: String, currVocabSize: Int): Unit = {
    particles.foreach { particle =>
      particle.unnormalizedReweight(currword, currVocabSize) }
  }

  def transitionAll (index: Int, words: Array[String], currVocabSize: Int,
                     docId: Int): Unit = {
    particles.foreach { particle =>
      particle.transition(index, words, currVocabSize,docId) }
  }

  def newDocumentUpdateAll (indexIntoSample: Int, doc: Array[String]): Unit = {
    particles.foreach { particle =>
      particle.newDocumentUpdate(indexIntoSample, doc) }
  }

  def resample (unnormalizedWeights: Array[Double]): Unit = {
    particles = multinomialResample(unnormalizedWeights)
  }

  def rejuvenateAll (wordIds: Array[(Int,Int)], batchSize: Int,
                     currVocabSize: Int): Unit = {
    particles.foreach { p => p.rejuvenate(wordIds, batchSize, currVocabSize) }
  }

  def uniformReweightAll (): Unit = {
    particles.foreach { p => p.weight = 1.0 / numParticles }
  }

  def printParticles (): Unit = particles.foreach { p => println(p) }

  /** TODO: DECIDE IF THIS IS NECESSARY */
  def rejuvenate (): Unit = { }

  /** Creates an array of particles resampled proportional to the weights */
  private def multinomialResample (unnormalizedWeights: Array[Double]):
  Array[Particle] = {
    val weightsCdf = Stats.normalizeAndMakeCdf(unnormalizedWeights)
    val resampledParticles = new Array[Particle](numParticles)
    (0 to numParticles-1).foreach {
      i =>
        val indexOfParticleToCopy = Stats.sampleCategorical(weightsCdf)
      resampledParticles(i) = particles(indexOfParticleToCopy).copy
    }
    resampledParticles
  }
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

  /** Rejuvenates particle by MCMC; we currently repeat the update step
   `batchSize` times. w is the *current* size of the vocabulary */
  def rejuvenate (wordIds: Array[(Int,Int)], batchSize: Int, w: Int): Unit = {
    val sample = Stats.sampleWithoutReplacement(wordIds, batchSize)
    for (i <- 0 to batchSize-1)
      sample.foreach { wordId => resampleRejuvSeqWord(wordId._1, wordId._2, w) }
  }

  /** Resamples a word in the rejuvenation sequence; w is *current* size of
   vocabulary*/
  def resampleRejuvSeqWord (docIdx: Int, wordIdx: Int, w: Int): Unit = {
    val doc = rejuvSeq(docIdx)
    val word = doc(wordIdx)
    val cdf = incrementalPosterior(wordIdx, docIdx, w)
    val sampledTopic = Stats.sampleCategorical(cdf)
    
    assignNewTopic(docIdx, wordIdx, sampledTopic)
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

  private def assignNewTopic (docIdx: Int, wordIdx: Int, newTopic: Int): Unit = {
    var docTopics = rejuvSeqAssignments(docIdx)
    var docUpdateVect = rejuvSeqDocVects(docIdx)
    val doc = rejuvSeq.getSampleSet()(docIdx)
    val word = doc(wordIdx)
    val oldTopic = docTopics(wordIdx)
    // should use indices to decrement old topic counts?
    globalVect.resampledUpdate(word, oldTopic, newTopic)
    docUpdateVect.resampledUpdate(wordIdx, oldTopic, newTopic)
    docTopics(wordIdx) = newTopic
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

  /** Generates normalized incremental update posterior approximation
   distribution P(z_j|Z_{i\j}, w_i); w is *current* size of the vocabulary. This
   normalized distribution is a distribution over all possible z_j in eqn 3 in
   Canini et al "Online Inference of Topics ..." */
  private def incrementalPosterior (wordIdx: Int, docId:Int,
                                    w: Int): Array[Double] = {
    var unnormalizedCdf = Array.fill(topics)(0.0)
    (0 to topics-1)foreach { i =>
      unnormalizedCdf(i) = incrementalEqn(wordIdx, docId, i, w) }
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

  /** For some word and some `topic`, calculates number proportional to
   p(z_j|Z_{i\j}, W_i). this is given as eqn (3) in Canini et al "Online
   Inference of Topics..." */
  private def incrementalEqn (wordIdx: Int, docId: Int, topic: Int,
                              w: Int): Double = {
    // We want to count the number of times a topic has occurred EXCLUDING the
    // current time it was assigned. This method just helps that goal.
    def counterHelper (count: Int, targetTopic: Int): Int = {
      if (targetTopic == topic) Math.max(count - 1, 0)
      else count
    }
    val doc = rejuvSeq.getSampleSet()(docId)
    val word = doc(wordIdx)
    val docTopics = rejuvSeqAssignments(docId)
    val priorTopic = docTopics(wordIdx)
    val docVect = rejuvSeqDocVects(docId)
    val globalUpdate =
      (counterHelper(globalVect.numTimesWordAssignedTopic(word, topic),
                     priorTopic) + beta) /
    (counterHelper(globalVect.numTimesTopicAssignedTotal(topic),
                   priorTopic) + w * beta)
    
    val docUpdate =
      (counterHelper(docVect.numTimesTopicOccursInDoc(topic),
                     priorTopic) + alpha) /
    (Math.max(docVect.wordsInDoc - 1, 0) + topics * alpha)
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
  
  def resampledUpdate (wordIdx: Int, oldTopic: Int, newTopic: Int): Unit = {
    timesTopicOccursInDoc(oldTopic) -= 1
    timesTopicOccursInDoc(newTopic) += 1
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

  def resampledUpdate (word: String, oldTopic: Int, newTopic: Int): Unit = {
    if (timesWordAssignedTopic((word, oldTopic)) == 1)
      timesWordAssignedTopic.remove((word, oldTopic))
    else timesWordAssignedTopic((word, oldTopic)) -= 1
    if (timesWordAssignedTopic contains (word, newTopic))
      timesWordAssignedTopic((word, newTopic)) += 1
    else
      timesWordAssignedTopic((word, newTopic)) = 1
    timesTopicAssignedTotal(oldTopic) -= 1
    timesTopicAssignedTotal(newTopic) += 1
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
