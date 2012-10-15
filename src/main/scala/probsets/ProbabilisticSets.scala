/** Probabilistic sets. Algorithms like reservoir sampling allow us to
 * probabilistically forget parts of a dataset. This package provides a
 * core of such algorithms for the purpose of probabilistically storing data
 * for LDA.
 */

package probsets

import scala.annotation.tailrec

import scala.util.{ Random => Random }

/** Core interface for probabilistic sets.
 */
abstract class ProbabilisticSet[T] () {
  def add (item: T)
  def get (i: Int): T
  def getSet (): Array[T]
}

/** Simple implementation of reservoir sampling.
 *
 * The classical treatment is given by Vitter in Random Sampling With a
 * Reservoir, but it's very well known at this point.
 *
 * WARNING: HIGHLY STATEFUL
 */
class ReservoirSampler[T: Manifest] (k: Int) extends ProbabilisticSet[T]() {
  var sample = new Array[T](k)
  var currIdx = 0
  var randombits = new Random()

  def add (item: T) = {
    if(currIdx >= k) {
      // IMPORTANT: `nextInt()` not inclusive, so the `+1` is required
      val randIdx = randombits.nextInt(currIdx+1)
      if (randIdx < k) sample(randIdx) = item
    }
    else sample(currIdx) = item
    
    currIdx += 1
  }

  def get (i: Int): T = sample(i)

  def getSet () = sample
}

/** Simple TEMPORARY tests used for dev purposes */
object simpletests {
  val r = new Random()
  
  def randomTest (setSz: Int, lstSz: Int): ProbabilisticSet[Int] = {
    def loop (i: Int, accu: ProbabilisticSet[Int]): ProbabilisticSet[Int] = {
      if (i == lstSz) accu
      else {
	accu.add(r.nextInt(9))
	loop(i+1, accu)
      }
    }
    var rs = new ReservoirSampler[Int](setSz)
    loop(0, rs)
  }
  
  def main (args: Array[String]) {
    println(randomTest(5, 15).getSet().deep.mkString(" "))
    println(randomTest(5, 15).getSet().deep.mkString(" "))
    println(randomTest(5, 15).getSet().deep.mkString(" "))
    println(randomTest(5, 15).getSet().deep.mkString(" "))
    println(randomTest(5, 15).getSet().deep.mkString(" "))
    println(randomTest(5, 15).getSet().deep.mkString(" "))
    println(randomTest(5, 15).getSet().deep.mkString(" "))
    println(randomTest(5, 15).getSet().deep.mkString(" "))
  }
}
