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

  var particles = new ParticleStore(T, alpha, beta, numParticles, ess,
                                    rejuvBatchSize, rejuvSeq)

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
    val now = System.currentTimeMillis
    (0 to Words.length-1).foreach{ i => processWord(i, Words, docId) }
    if (Words.length != 0)
      print("\t\t\t" + ((System.currentTimeMillis - now)/Words.length))
    println()
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
    particles.normalizeWeights()

    if (particles.shouldRejuvenate())
      particles.rejuvenate(allWordIds(), currVocabSize)
  }

  private def newDocumentUpdate (doc: Array[String]): Int = {
    val index = rejuvSeq.addItem(doc)
    particles.newDocumentUpdateAll(index, doc)
    index
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

  // print total percentage a word as it occurs in each particular topic
  def printTopics (): Unit = {
    val wrdWIdx = vocabToId.toArray[(String,Int)]
    val particleObjs = particles.particles


    for (p <- 0 to particleObjs.length-1) {
      println("PARTICLE " + p)
      val countVctr = particleObjs(p).globalVect
      for (t <- 0 to T-1) {
        val percs = new Array[(Double,String)](wrdWIdx.size)
        for (i <- 0 to wrdWIdx.size-1) {
          // grab each word, compute how much it comprises a given topic
          val (w,id) = wrdWIdx(i)
          val prctg = countVctr.numTimesWordAssignedTopic(w, t).toDouble /
            countVctr.numTimesTopicAssignedTotal(t)
          percs(i) = (prctg, w);
        }
        println("topic " + t)
        println("\t" + percs.sorted.reverse.deep.mkString("\n\t"))
      }
      println
    }
  }
}
