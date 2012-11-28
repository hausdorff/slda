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

/** Wrangles the 20 Newsgroups dataset
 */
object TNG {
  
  def main (args: Array[String]) {
    val f = new File("data/20_newsgroups/alt.atheism")
    val arr = f.listFiles
    println(Io.lines(arr(1)))
  }
}
