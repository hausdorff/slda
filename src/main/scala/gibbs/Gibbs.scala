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
 * wAssignedZ, 2D array, where `wAssignedZ(i)(j)` represents the number of
 *             times word `w(j)` is assigned to `topic(i)`
 *
 * @param docs Collection of D documents, each doc a string
 * @param T number of topics
 * @param prior Becomes the symmetric Dirichlet priors
 */
abstract class Gibbs (val docs: Array[String], val T: Int,
		      val prior: Double) {
  // CORE VARIABLES
  val alpha = prior
  val beta = prior
  val D = docs.length
  val (w, d) = Text.bow(docs)
  val N = w.length
  var z = Array.fill(N)(new Random().nextInt(T))
  val W = wIdx.size
  
  // BOOKKEEPING VARIABLES
  val wIdx = canonicalWordIndices(w)
  var wAssignedZ = initWAssignedZ(w, z, wIdx)
  var assignedZInD = initWAssignedZ(w, z, wIdx)
  
  /** Maps words to canonical indices; useful for maintaining word counts
   * over sets or subsets of documents.
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
  
  /** Produces TxW matrix, where T is the number of topics, and W is the
   * size of the vocabulary. `wAssignedZ(i)(j)` will return the number of
   * times word `w(j)`, is assigned topic `z(i)`.
   *
   * WARNING: MUTATES STATE
   */
  private def initWAssignedZ (w: Array[String], z: Array[Int],
			      wIdx: HashMap[String,Int]) = {
    @tailrec
    def loop (i: Int, wAssignedZ: Array[Array[Int]]): Array[Array[Int]] = {
      if (i >= w.length) wAssignedZ
      else {
	wAssignedZ(z(i))(i) += 1
	loop(i+1, wAssignedZ)
      }
    }
    var wAssignedZ = Array.fill(T, wIdx.size)(0)
    loop(0, wAssignedZ)
  }

  /** Produces a TxD matrix, where T is the number of topics, and D is the
   * number of documents. `assignedZInD(i)(j)` will return the number of
   * words `d(j)` that are assigned `z(i)`
   *
   * WARNING: MUTATES STATE
   */
  private def initAssignedZInD (z: Array[Int], d: Array[Int],
				wIdx: HashMap[Int,Int]) = {
    @tailrec
    def loop (i: Int, assignedZInD: Array[Array[Int]]):
    Array[Array[Int]] = {
      if (i >= w.length) assignedZInD
      else {
	assignedZInD(z(i))(i) += 1
	loop(i+1, assignedZInD)
      }
    }
    var assignedZInD = Array.fill(T, D)(0)
    loop(0, assignedZInD)
  }
}

/** A collapsed batch Gibbs sampler
 */
class CollapsedGibbs (docs: Array[String], T: Int, prior: Double)
extends Gibbs(docs, T, prior) {
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
  def main (args: Array[String]) = {
    // Test that the objects gets made n stuff
    val cg = new CollapsedGibbs(Array("cows are green", "birds are blue"),
				3, 0.3)
    println("w: \"" + cg.w.deep.mkString("\" \"") + "\"")
    println("d: " + cg.d.deep.mkString(" "))
    println("z: " + cg.z.deep.mkString(" "))
  }
}
