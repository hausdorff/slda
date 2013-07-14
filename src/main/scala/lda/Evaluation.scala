/** Provides different techniques for evaluating a run of LDA.
 */

package evaluation

import scala.collection.mutable.{ ArrayBuffer => ArrayBuffer }
import scala.math

import lda._


object Evaluation {
  def mi (ourLabels: ArrayBuffer[Int], theirLabels: Array[String],
          topics: Int, theirLabelTypes: Collection[String]):
  Double = {
    val n = ourLabels.length.toDouble
    var s = 0.0
    val outcomes = ourLabels.zip(theirLabels)
    for (t <- 0 until topics) {
      for (theirlabel <- theirLabelTypes) {
        val inter =
          outcomes.filter { x => x._1 == t && x._2 == theirlabel}.length
        if (inter > 0) {
          val allTheirs = theirLabels.filter { x => x == theirlabel }.size
          val allOurs = ourLabels.filter { x => x == t }.size
          val logterm = (n * inter) / (allTheirs.toDouble * allOurs)
          s += (inter / n) * (math.log(logterm) / math.log(2))
        }
      }
    }

    s
  }

  def entropies (ourLabels: ArrayBuffer[Int], theirLabels: Array[String],
                 topics: Int, theirLabelTypes: Collection[String]):
  Double = {
    val n = ourLabels.length.toDouble
    var ent1 = 0.0
    for (theirLabel <- theirLabelTypes) {
      val pred = theirLabels.filter { x => x == theirLabel }.length / n
      ent1 += pred * (math.log(pred) / math.log(2))
    }
    ent1 = -ent1

    var ent2 = 0.0
    for (t <- 0 until topics) {
      val pred = ourLabels.filter { x => x == t }.length / n
      ent2 += pred * (math.log(pred) / math.log(2))
    }
    ent2 = -ent2

    (ent1 + ent2)
  }

  def nmi (model: PfLda, labels: Array[String],
           labelTypes: Collection[String]): Array[Double] = {
    val ntopics = model.T
    val ps = model.particles
    val ourLabels = ps.particles.map { p => p.docLabels }
    val mis = ourLabels.map { docLabels =>
      2* mi(docLabels, labels, ntopics, labelTypes) /
         entropies(docLabels, labels, ntopics, labelTypes) }
    mis
  }

  def writeOut (model: PfLda, labels: Array[String],
                labelTypes: Collection[String], filename: String): Unit = {
    val nmis = nmi(model, labels, labelTypes)
    println(nmis.deep)
    val writer = new PrintWriter(new File(filename))

    for (e <- nmis) {
      writer.write(e + "\n")
    }
    writer.close()
  }
}
