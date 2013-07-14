package lda

import scala.util.Random

import evaluation._
import wrangle._

object Sim3PfParams {
  val alpha = 0.1
  val beta = 0.1
  val smplSize = 2980
  val numParticles = 100
  val ess = 3
  val rejuvBatchSize = 100
  val rejuvMcmcSteps = 20
}

object RunLda {
  def main (args: Array[String]) {
    println("loading corpus...")
    var (corpus, labels, cats) = wrangle.TNG.sim3
    // if we don't shuffle them, and if we don't shuffle them with the same seed,
    // our NMI suffers greatly
    corpus = (new Random(10)).shuffle(corpus.toSeq).toArray
    labels = (new Random(10)).shuffle(labels.toSeq).toArray
    println("building model...")
    val model = new PfLda(cats, Sim3PfParams.alpha, Sim3PfParams.beta,
                          Sim3PfParams.smplSize, Sim3PfParams.numParticles,
                          Sim3PfParams.ess, Sim3PfParams.rejuvBatchSize,
                          Sim3PfParams.rejuvMcmcSteps)

    println("running model...")
    println("DOCUMENT\t\t\tTIME CONSUMPTION PER WORD (MILLISECONDS)")
    for (i <- 0 to corpus.length-1) {
      print(i + " / " + corpus.length)
      //val now = System.nanoTime
      //println("doc " + i + " / " + (corpus.length-1))
      model.ingestDoc(corpus(i))
      // TODO: REMOVE HACKY TIMING CODE FOR BENCHMARKING IMPROVEMENTS
      //println(i + " " + (System.nanoTime - now))
      if (i % 100 == 0) {
        Evaluation.writeOut(model, labels.slice(0, i),
                            DataConsts.SIM_3_LABELS.slice(0, i),
                            DataConsts.RESULTS_DIR +  i.toString() + ".txt")
      }
    }
    model.writeTopics("results.txt")

    val mis = Evaluation.nmi(model, labels, wrangle.DataConsts.SIM_3_LABELS)
    println(mis.deep)
  }
}
