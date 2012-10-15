/** Gibbs Sampling for LDA
 */

package gibbs

import scala.annotation.tailrec

import scala.util.{ Random => Random }
import scala.collection.mutable.{ HashMap => HashMap }

/** A simple Gibbs sampler interface
 *
 * Different types of Gibbs samplers (eg, O-LDA, collapsed, incremental,
 * etc.) will be derived from this abstract class.
 *
 * Note that internal variables are named to be consistent with the
 * notation of Canini et al in Online Inference of Topics with LDA.
 * The guts are as follows:
 *
 * CORE VARIABLES
 * D, the number of documents
 * N, the number of (non-unique!) words in all the documents
 * w, the vector of word occurrences in all documents
 * d, `d(i)` is the document for the word `w(i)`
 * z, `z(i)` is the topic assignment for the word `w(i)`
 * W, the size of the vocabulary
 *
 * BOOKKEEPING VARIABLES
 * wIdx, maps words to canonical indices; used to keep track of word counts
 * allAssignedZ, T-length vector storing number of times any word is
 *               assigned each of the topics
 * wAssignedZ, TxW array, where `wAssignedZ(i)(wIdx(j))` represents the
 *             number of times word `w(j)` is assigned to `topic(i)`
 * allAssignedZInD, TxD matrix, where T `allAssignedZInD(i)(d(j))`
 *                  returns number of words in document `d(j)` that are
 *                  assigned topic `z(i)`
 *
 * @param docs Collection of D documents, each doc a string
 * @param T number of topics
 * @param prior Becomes the symmetric Dirichlet priors
 */
abstract class Gibbs (val docs: Array[String], val T: Int,
		      val prior: Double) {
  val alpha = prior
  val beta = prior
  val D = docs.length
  val (w, d) = Text.bow(docs)
  val N = w.length
  var z = Array.fill(N)(new Random().nextInt(T))
  val wIdx = canonicalWordIndices(w)
  val W = wIdx.size
  var (allAssignedZ, wAssignedZ, allAssignedZInD) =
    assignmentMatrices(w, d, z, wIdx)
  
  def resampleTopic ()
  def perplexity (docs: Array[String]): Double
  
  /** Builds map w -> {W}, mapping every word to a unique integer in the
   * range from 0-W. This is useful for maintaining counts of words over
   * documents.
   */
  private def canonicalWordIndices (w: Array[String]) = {
    @tailrec
    def loop (i: Int, currIdx: Int, accu: HashMap[String,Int]):
    HashMap[String,Int] = {
      if (i >= w.length) accu
      else {
	val currWord = w(i)
	if (accu contains currWord) loop(i+1, currIdx, accu)
	else loop(i+1, currIdx+1, accu += (currWord -> currIdx))
      }
    }
    loop(0, 0, new HashMap[String,Int])
  }
  
  /** Produces `allAssignedZ`, `wAssignedZ` and `allAssignedZInD`.
   *
   * `allAssignedZ` is a vector of length T, which counts the number of
   * times any word is assigned topic Z in the corpus.
   *
   * `wAssignedZ` is a TxW matrix, where T is the number of topics, and W is
   * the size of the vocabulary. `wAssignedZ(i)(wIdx(j))` will return the
   * number of times word `w(j)`, is assigned topic `z(i)`.
   *
   * `allAssignedZInD` is a TxD matrix, where T is the number of topics,
   * and D is the number of documents. `allAssignedZInD(i)(d(j))` will
   * return the number of words `d(j)` that are assigned `z(i)`
   *
   * WARNING: MUTATES STATE
   */
  private def assignmentMatrices (w: Array[String], d: Array[Int],
				  z: Array[Int],
				  wIdx: HashMap[String,Int]) = {
    @tailrec
    def loop (i: Int, allAssignedZ: Array[Int],
	      wAssignedZ: Array[Array[Int]],
	      allAssignedZInD: Array[Array[Int]]):
    (Array[Int], Array[Array[Int]], Array[Array[Int]]) = {
      if (i >= w.length) (allAssignedZ, wAssignedZ, allAssignedZInD)
      else {
	val wordIdentifier = wIdx(w(i))
	val topicIdentifier = z(i)
	val docIdentifier = d(i)
	allAssignedZ(topicIdentifier) += 1
	wAssignedZ(topicIdentifier)(wordIdentifier) += 1
	allAssignedZInD(topicIdentifier)(docIdentifier) += 1
	loop(i+1, allAssignedZ, wAssignedZ, allAssignedZInD)
      }
    }
    var allAssignedZ = Array.fill(T)(0)
    var wAssignedZ = Array.fill(T, W)(0)
    var allAssignedZInD = Array.fill(T, D)(0)
    loop(0, allAssignedZ, wAssignedZ, allAssignedZInD)
  }
}

/** A collapsed batch Gibbs sampler
 */
class CollapsedGibbs (docs: Array[String], T: Int, prior: Double)
extends Gibbs(docs, T, prior) {
  val selector = new Random()
  
  /** Computes the update step for 1 choice of topic
   *
   * In "Online Inference of Topics with LDA" (Canini et al), equation (1)
   * gives us the Gibbs update step, wherein we are resampling our topics
   * based on the conditional probability P(z_j | \vec{z}_{N\j}, \vec{w}_N).
   *
   * This method computes the UNNORMALIZED distribution for A SINGLE CHOICE
   * OF z_j. In order to obtain the complete distribution described above,
   * you must call `resampleTopic()`, which will call this method for all
   * possible choices of z_j. This gives a complete specification of the
   * conditional distribution described above.
   *
   * TO CALL THIS METHOD YOURSELF: typically you will hold parameters
   * `currWord`, `currTopic`, and `currDoc` CONSTANT, and iterate through
   * all possible values of `newTopic`. Here `newTopic` is our choice of ONE
   * specific z_j.
   *
   * Here is a spec of each parameter:
   * @param currWord Unique identifier specifying the current word we're
   *                 resampling
   * @param newTopic Our choice of z_j
   * @param currTopic The topic associated with `currWord`
   * @param currDoc The unique identifier specifying the document of
   *                `currWord`
   */
  private def pointPosterior (currWord: Int, newTopic: Int, currTopic: Int,
			      currDoc: Int): Double = {
    if (currTopic == newTopic) {
      // Pr[currWord | topic]
      val frst = (wAssignedZ(newTopic)(currWord) - 1 + beta) /
      (allAssignedZ(newTopic) - 1 + W)
      // Pr[topic | currDoc]
      val scnd = (allAssignedZInD(newTopic)(currDoc) - 1 + alpha) /
      (docs(currDoc).length - 1)
      
      val res = frst * scnd
      res
    }
    else {
      // Pr[currWord | topic]
      val frst = (wAssignedZ(newTopic)(currWord) + beta) /
      (allAssignedZ(newTopic) - 1 + W)
      // Pr[topic | currDoc]
      val scnd = (allAssignedZInD(newTopic)(currDoc) + alpha) /
      (docs(currDoc).length - 1)
      
      val res = frst * scnd
      res
    }
  }

  /** Resamples the topic of the word that occurs at position `index` in
   * the corpus
   */
  private def resampleTopic (index: Int): Int = {
    val currWord = wIdx(w(index))
    val currTopic = z(index)
    val currDoc = d(index)
    
    // MUTATES ACCUMULATOR
    @tailrec
    def loop (i: Int, limit: Int, accu: Array[Double]): Array[Double] = {
      if (i >= limit) accu
      else {
	accu(i) = pointPosterior(currWord, i, currTopic, currDoc)
	loop(i+1, limit, accu)
      }
    }
    
    var topicDistr = Array.fill(T)(0.0)
    loop(0, T, topicDistr)
    //println("topicDistr " + topicDistr.deep.mkString(" "))

    Stats.sampleDiscreteContiguousCDF(Stats.normalize(topicDistr))
  }
  
  /** Calculates perplexity, almost always of a test set.
   *
   * Perplexity, used by convention in the language modeling community, is
   * algebraically equivalent to the inverse geometric mean of per-word
   * likelihood. Thus, a lower perplexity indicates better generalization
   * performance.
   */
  def perplexity (docs: Array[String]): Double = {
    throw new Exception("NOT IMPLEMENTED")
  }
  
  /** Randomly resamples a word in the corpus
   */
  def resampleTopic () {
    val randomWordIdx = selector.nextInt(N)
    val newTopic = resampleTopic(randomWordIdx)
    throw new Exception("NOT IMPLEMENTED -- YOU HAVE SAMPLED A NEW TOPIC " +
		      "BUT NOT UPDATED YOUR COUNTS!")
  }
}

object Stats {
  val sampler = new Random()
  
  def normalize (arr: Array[Double]): Array[Double] = {
    val s = arr.reduceLeft(_+_)
    arr.map(x => x / s)
  }
  
  
  def sampleDiscreteContiguousCDF (cdf: Array[Double]): Int = {
    val r = sampler.nextDouble()
    @tailrec
    def loop (currIdx: Int): Int = {
      if (currIdx+1 >= cdf.length || r > cdf(currIdx)) currIdx
      else loop(currIdx+1)
    }
    loop(0)
  }
}

/** Simple functions for processing text */
object Text {
  def tokenize (s: String): Array[String] = { s.split("\\s+") }
  
  /** Converts documents into a single array of words
   *
   * Takes `docs`, our array of documents, breaks each doc into an array
   * of words, and then smashes all those arrays together into a single
   * array.
   *
   * Additionally, we return an array that maps each word to the document
   * it came from, ie, the `word[i]` will have come from document
   * `assignment[i]`
   *
   * @return An array of words 
   */
  def bow (docs: Array[String]): (Array[String], Array[Int]) = {
    @tailrec
    def loop (i: Int, accuDocs: Array[String], accuAssig: Array[Int]):
    (Array[String], Array[Int]) = {
      if (i == docs.length) (accuDocs, accuAssig)
      else {
	val nextDocs = tokenize(docs(i))
	val nextAssig = Array.fill(nextDocs.length)(i)
	loop(i + 1, accuDocs ++ nextDocs, accuAssig ++ nextAssig)
      }
    }
    val initAccuDocs = tokenize(docs(0))
    val initAccuAssig = Array.fill(initAccuDocs.length)(0)
    if (docs.length == 1) (initAccuDocs, initAccuAssig)
    else loop(1, initAccuDocs, initAccuAssig)
  }
}

/** Simple, TEMPORARY tests for development purposes */
object TestGibbs {
  def repeat (i: Int, n: Int, cg: CollapsedGibbs): Unit = {
    if (i == n) cg.resampleTopic()
    else {
      cg.resampleTopic()
      repeat(i+1, n, cg)
    }
  }
  
  def main (args: Array[String]) = {
    // Test that the objects gets made n stuff
    val cg = new CollapsedGibbs(Array("cows are green", "birds are blue"),
				3, 0.3)
    repeat(0, 100, cg)
    println("w: \"" + cg.w.deep.mkString("\" \"") + "\"")
    println("d: " + cg.d.deep.mkString(" "))
    println("z: " + cg.z.deep.mkString(" "))
    println("allAssignedZ: " + cg.allAssignedZ.deep.mkString(" "))
    println("wAssignedZ: " + cg.wAssignedZ.deep.mkString(" "))
    println("allAssignedZInD" + cg.allAssignedZInD.deep.mkString(" "))
    cg.resampleTopic()
  }
}
