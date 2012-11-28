/** Wrangles 20newgroups corpus for LDA
 */

package wrangle

import scala.io.Source
import scala.annotation.tailrec
import java.io.File


/** Handles basic IO for our little package */
object Io {
  def filesInDir (dir: String) = {
    val f = new File(dir)
    for (fi <- f.listFiles)
      yield fi
  }

  /* Takes filename, returns List of String, each string a line in file */
  def lines (fname: java.io.File): List[String] = {
    def loop (iter: Iterator[String]): List[String] = {
      if (iter.hasNext) iter.next :: loop(iter)
      else List()
    }
    loop(Source.fromFile(fname).getLines)
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

/** Wrangles the 20 Newsgroups dataset
 */
object TNG {
  
  def main (args: Array[String]) {
    val f = new File("data/20_newsgroups/alt.atheism")
    val arr = f.listFiles
    println(Io.lines(arr(1)))
  }
}
