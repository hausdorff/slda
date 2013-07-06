package lda

import wrangle._

object Sim3PfParams {
  val alpha = 0.1
  val beta = 0.1
  val smplSize = 2980
  val numParticles = 100
  val ess = 1
  val rejuvBatchSize = 30
  val rejuvMcmcSteps = 20
}

object RunLda {
  def main (args: Array[String]) {
    println("loading corpus...")
    val (corpus, sws, cats) = wrangle.TNG.sim3
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
    }
    model.writeTopics("results.txt")
  }
}
