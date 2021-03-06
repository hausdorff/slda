/** Gibbs Sampling for LDA
 */

package gibbs

import scala.math
import scala.annotation.tailrec
import scala.util.{ Random => Random }
import scala.collection.mutable.{ HashMap => HashMap }

import lda.Stats
import stream._
import wrangle._

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
 * w, `w(i)` is the i-th word in the corpus
 * d, `d(i)` is the document for the i-th word in the corpus
 * z, `z(i)` is the topic assignment of the i-th word in the corpus
 * W, the size of the vocabulary
 *
 * BOOKKEEPING VARIABLES
 * wIdx, `wIdx(w(i))` is the canonical identifier (an `Int`) of the i-th word
 *       in the corpus; used to keep track of word counts
 * allAssignedZ, T-length vector storing number of times any word is
 *               assigned each of the topics
 * wAssignedZ, TxW array, where `wAssignedZ(i)(wIdx(w(j)))` represents the
 *             number of times word `w(j)` is assigned to topic `z(i)`
 * allAssignedZInD, TxD matrix, where T `allAssignedZInD(i)(d(j))`
 *                  returns number of words in document `d(j)` that are
 *                  assigned topic `z(i)`
 *
 * @param docs Collection of D documents, each doc a string
 * @param T number of topics
 * @param alpha Symmetric Dirichlet prior
 * @param beta Symmetric Dirichlet prior
 */
abstract class Gibbs (val docs: Array[String], val T: Int,
		      val alpha: Double, val beta: Double) {
  val D = docs.length
  val whitelist = Text.stopWords(DataConsts.TNG_WHITELIST)
  val (w, d) = Text.bow(docs, (str: String) => whitelist(str))
  val N = w.length
  var z = Array.fill(N)(new Random().nextInt(T))
  val wIdx = canonicalWordIndices(w)
  val W = wIdx.size
  var (allAssignedZ, wAssignedZ, allAssignedZInD) =
    assignmentMatrices(w, d, z, wIdx)

  def this (docs: Array[String], T: Int, alpha: Double, beta: Double,
	    allAssignedZ: Array[Int], wAssignedZ: Array[Array[Int]],
	    allAssignedZInD: Array[Array[Int]]) = {
    this(docs, T, alpha, beta)
    this.allAssignedZ = GibbsUtil.copy1dArr(allAssignedZ)
    this.wAssignedZ = GibbsUtil.copy2dArr(wAssignedZ)
    this.allAssignedZInD = GibbsUtil.copy2dArr(allAssignedZInD)
  }

  def pointPosterior (currWord: Int, newTopic: Int, currTopic: Int,
		      currDoc: Int): Double
  
  def resampleTopic(): Unit = resampleTopic(pointPosterior)

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
  def assignmentMatrices (w: Array[String], d: Array[Int],
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
  
  /** Resamples the topic of the word that occurs at position `index` in
   * the corpus
   */
  private def resampleTopic (index: Int, resampler: (Int, Int, Int, Int)
			   => Double): Int = {
    val currWord = wIdx(w(index))
    val currTopic = z(index)
    val currDoc = d(index)
    
    // Builds unnormalized topic distribution
    @tailrec
    def loop (i: Int, limit: Int, accu: Array[Double]): Array[Double] = {
      if (i >= limit) accu
      else {
	//accu(i) = pointPosterior(currWord, i, currTopic, currDoc)
	accu(i) = resampler(currWord, i, currTopic, currDoc)
	loop(i+1, limit, accu)
      }
    }
    
    var topicDistr = Array.fill(T)(0.0)
    loop(0, T, topicDistr)
    //println("topicDistr " + topicDistr.deep.mkString(" "))

    Stats.sampleCategorical(Stats.normalizeAndMakeCdf(topicDistr))
  }
  
  /** Resamples one complete assignment for the corpus
   */
  def resampleTopic (resampler: (Int, Int, Int, Int) => Double): Unit = {
    @tailrec
    def loop (i: Int): Unit = {
      if (i >= w.length) return
      else {
	val word = w(i)
	val canonWordIdx = wIdx(word)
	val doc = d(i)
	val oldTopic = z(i)
	val newTopic = resampleTopic(i, resampler)
	
	z(i) = newTopic
	allAssignedZ(oldTopic) -= 1
	allAssignedZ(newTopic) += 1
	wAssignedZ(oldTopic)(canonWordIdx) -= 1
	wAssignedZ(newTopic)(canonWordIdx) += 1
	allAssignedZInD(oldTopic)(doc) -= 1
	allAssignedZInD(newTopic)(doc) += 1
	
	loop(i+1)
      }
    }
    loop(0)
  }
}

/** A collapsed batch Gibbs sampler for performing LDA
 */
class CollapsedGibbs (docs: Array[String], T: Int, alpha: Double,
		      beta: Double, k: Int)
extends Gibbs(docs, T, alpha, beta) {
  var sampler = new ReservoirSampler[Array[String]](k)
  
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
  def pointPosterior (currWord: Int, newTopic: Int, currTopic: Int,
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

  def evaluator (): Evaluator = {
    // Generate new assignment matrices because the evaluator needs them
    // for its own Gibbs sampling
    var (evAllAssignedZ, evWAssignedZ, evAllAssignedZInD) =
      assignmentMatrices(w, d, z, wIdx)
    new Evaluator(docs, T, alpha, beta, evAllAssignedZ, evWAssignedZ,
		  evAllAssignedZInD)
  }
}

class Evaluator (docs: Array[String], T: Int,
		 alpha: Double, beta: Double, allAssignedZ: Array[Int],
		 wAssignedZ: Array[Array[Int]],
		 allAssignedZInD: Array[Array[Int]])
extends Gibbs(docs, T, alpha, beta, allAssignedZ, wAssignedZ,
	      allAssignedZInD) {
  def pointPosterior (currWord: Int, newTopic: Int, currTopic: Int,
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
}

object GibbsUtil {
  def copy1dArr [T:Manifest](arr: Array[T]): Array[T] = {
    val newarr = new Array[T](arr.length)
    Array.copy(arr, 0, newarr, 0, arr.length)
    newarr
  }
  
  def copy2dArr [T:Manifest](arr: Array[Array[T]]): Array[Array[T]] = {
    val newarr = new Array[Array[T]](arr.length)
    for (i <- 0 to (arr.length-1)) {
      newarr(i) = new Array[T](arr(i).length)
      Array.copy(arr(i), 0, newarr(i), 0, arr(i).length)
    }
    newarr
  }
}

