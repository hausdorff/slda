/** Gibbs Sampling for LDA
 */

package gibbs

/** A simple collapsed Gibbs sampler
 *
 * Note that internal variables are named to be consistent with the
 * notation of Canini et al in Online Inference of Topics with LDA.
 *
 * @param docs Collection of D documents, each doc a string
 * @param T number of topics
 */
class CollapsedGibbs (val docs: Array[String], val T: Int, alpha: Double,
		   beta: Double) {
  val D = docs.length
  val bow = new BOW(docs)
  val N = bow.N
}

/** A simple bag of words for use with `CollapsedGibbs`
 *
 * @param docs Collection of D documents, each doc a string
 */
class BOW (val docs: Array[String]) {
  val (w, assignments) = docsToBOW()
  val N = w.length
  
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
  private def docsToBOW (): (Array[String], Array[Int]) = {
    throw new Exception("NOT IMPLEMENTED")
  }
}

/** Simple functions for processing text */
object Text {
  def tokenize (s: String) { throw new Exception("NOT IMPLEMENTED") }
}

/** Simple, TEMPORARY tests for development purposes */
object TestGibbs {
  def main (args: Array[String]) = {
    // Test that the objects gets made n stuff
    val cg = new CollapsedGibbs(Array("cow", "cows"), 10, 0.1, 0.2)
    println(cg.bow)
  }
}
