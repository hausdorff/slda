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
 * @param rejuvBatchSize number of words to rejuv per rejuvenation step
 * @param rejuvMcmcSteps number of steps to run rejuv MCMC before stopping
 */
class PfLda (val T: Int, val alpha: Double, val beta: Double,
             val smplSize: Int, val numParticles: Int, ess: Double,
             val rejuvBatchSize: Int, val rejuvMcmcSteps: Int) {
  val Whitelist = Text.stopWords(DataConsts.TNG_WHITELIST)
  var vocabToId = HashMap[String,Int]()
  var rejuvSeq = new ReservoirSampler[Array[String]](smplSize)
  var currVocabSize = 0
  var currWordIdx = 0

  var particles = new ParticleStore(T, alpha, beta, numParticles, rejuvSeq)

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

    particles.unnormalizedReweightAll(currword, currVocabSize)
    particles.transitionAll(i, words, currVocabSize, docId)
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
    particles.newDocumentUpdateAll(index, doc)
    index
  }
  
  private def rejuvenate (): Unit = {
    val now = System.currentTimeMillis
    // resample the particles; 
    particles.resample(particleWeightArray())
    // TODO: HACKY TIMING CODE, REMOVE LATER
    println("\t" + (System.currentTimeMillis - now))
    // pick rejuvenation sequence in the reservoir
    val wordIds = allWordIds()
    particles.rejuvenateAll(wordIds, rejuvBatchSize, currVocabSize)
    particles.uniformReweightAll()
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
    for (i <- 0 to numParticles-1) particles.particles(i).weight = weights(i)
  }

  /** Helper method puts the weights of particles into an array, so that
   `particles(i) == weights(i)` */
  private def particleWeightArray (): Array[Double] = {
    var weights = Array.fill(numParticles)(0.0)
    for (i <- 0 to numParticles-1) weights(i) = particles.particles(i).weight
    weights
  }
}
