/** Wrangles 20newgroups corpus for LDA
 */

package wrangle

import scala.io.Source
import scala.annotation.tailrec
import java.io.File


/** Handles basic IO for our little package */
object Io {
  def files (dir: String): Array[java.io.File] = {
    val f = new File(dir)
    val files = f.listFiles
    if (files == null)
      throw new RuntimeException("No files in data directory: " + dir)
    else files
  }

  /** Transforms contents of a file into a single String */
  def fileToString (f: java.io.File): String = Source.fromFile(f).mkString

  /** Returns all files in corpus as an array of Strings */
  def rawCorpus (dir: String): Array[String] = {
    val fs = files(dir)
    @tailrec
    def loop (i: Int, acc: Array[String]): Array[String] = {
      if (i >= fs.length) acc
      else {
        acc(i) = fileToString(fs(i))
        loop(i+1, acc)
      }
    }
    loop(0, new Array[String](fs.length))
  }

  def rawCorpus (dirs: List[String]): Array[String] = {
    def loop (li: List[String]): Array[String] = li match {
      case Nil => Array()
      case hd::Nil => rawCorpus(hd)
      case hd::tl => rawCorpus(hd) ++ loop(tl)
    }
    loop(dirs)
  }
}

/** Simple functions for processing text */
object Text {
  val WHITESPACE = "\\s+"
  
  /** Tokenizes a document, removing everything not a stopwords filter */
  def tokenize (s: String, filter: String => Boolean): Array[String] =
    s.split(WHITESPACE).filter(filter)

  /** Generates a Set that contains stop words from a file */
  def stopWords (fname: String): Set[String] = {
    Io.fileToString(new File(fname)).split(WHITESPACE).toSet
  }
  
  /** Converts documents into a single array of words
   *
   * Takes `docs`, our array of documents, breaks each doc into an array
   * of words, and then smashes all those arrays together into a single
   * array.
   *
   * Additionally, we return an array that maps each word to the document
   * it came from, ie, the word `accuDocs(i)` will have come from document
   * `accuAssig[i]`
   *
   * @return accuDocs array of words
   * @return accuAssig array of Ints -- accuAssig(i) is document # of ith wrd
   */
  def bow (docs: Array[String], filter: String => Boolean):
  (Array[String], Array[Int]) = {
    @tailrec
    def loop (i: Int, accuDocs: Array[String], accuAssig: Array[Int]):
    (Array[String], Array[Int]) = {
      if (i == docs.length) (accuDocs, accuAssig)
      else {
        val nextDocs = tokenize(docs(i), filter)
        val nextAssig = Array.fill(nextDocs.length)(i)
        loop(i + 1, accuDocs ++ nextDocs, accuAssig ++ nextAssig)
      }
    }
    val initAccuDocs = tokenize(docs(0), filter)
    val initAccuAssig = Array.fill(initAccuDocs.length)(0)
    if (docs.length == 1) (initAccuDocs, initAccuAssig)
    else loop(1, initAccuDocs, initAccuAssig)
  }

  /** Wrapper simply returns the tokenized document */
  def bow (doc: String, filter: String => Boolean): Array[String] = {
    val (words, docs) = bow(Array(doc), filter)
    words
  }
}

object DataConsts {
  val DATA_DIR = "data/"
  val TNG_TRAIN_DIR = DATA_DIR + "20news-bydate-train/"
  val TNG_WHITELIST = DATA_DIR + "TNG_WHITELIST"
  val ALT_ATHEISM = TNG_TRAIN_DIR + "alt.atheism"
  val SIM_3_TRAIN_DOCS = List("comp.graphics", "comp.os.ms-windows.misc",
                              "comp.windows.x") map (s => TNG_TRAIN_DIR + s)
  val REL_3_TRAIN_DOCS = List("talk.politics.misc", "talk.politics.guns",
                              "talk.politics.mideast") map (s =>
                                TNG_TRAIN_DIR + s)
  val DIFF_3_TRAIN_DOCS = List("alt.atheism", "rec.sport.baseball",
                               "sci.space") map (s => TNG_TRAIN_DIR + s)
}

/** Wrangles the 20 Newsgroups dataset
 */
object TNG {
  
  def main (args: Array[String]) {
    val corpus = Io.rawCorpus(DataConsts.ALT_ATHEISM)
    println("Docs in corpus " + corpus.length)
    val sws = Text.stopWords(DataConsts.TNG_WHITELIST)
    val (w, d) = Text.bow(corpus, (str: String) => sws(str))
    val (wp, dp) = Text.bow(corpus, (str: String) => true)
    println("Words in first doc " + corpus(0).length)
    println("Vocabulary w filtering\t" + w.length)
    println("Vocabulary wo filtering\t" + wp.length)
    
    val sim3 = Io.rawCorpus(DataConsts.SIM_3_TRAIN_DOCS)
    val rel3 = Io.rawCorpus(DataConsts.REL_3_TRAIN_DOCS)
    val diff3 = Io.rawCorpus(DataConsts.DIFF_3_TRAIN_DOCS)
    println(sim3.length)
    println(rel3.length)
    println(diff3.length)

    println(Io.files(DataConsts.TNG_TRAIN_DIR + "rec.sport.baseball").length)
  }
}
