/** Probabilistic sets. Algorithms like reservoir sampling allow us to
 * probabilistically forget parts of a dataset. This package provides a
 * core of such algorithms for the purpose of probabilistically storing data
 * for LDA.
 */

package stream

import scala.annotation.tailrec

import scala.util.{ Random => Random }

/** Core interface for probabilistic sets.
 */
abstract class AssociativeStreamSampler[T] {
  def add (item: T): AssociativeStreamSampler[T]
  def addAll (items: Array[T]): AssociativeStreamSampler[T]
  def size (): Int
  def apply (i: Int): T
  def getSampleSet (): Array[T]
  def size (): Int
}

/** Simple implementation of reservoir sampling.
 *
 * The classical treatment is given by Vitter in Random Sampling With a
 * Reservoir, but it's very well known at this point.
 *
 * WARNING: HIGHLY STATEFUL
 */
class ReservoirSampler[T: Manifest] (k: Int) extends
AssociativeStreamSampler[T] {
  var sample = new Array[T](k)
  var currIdx = 0
  var randombits = new Random()

  def add (item: T): ReservoirSampler[T] = {
    if(currIdx >= k) {
      // IMPORTANT: `nextInt()` not inclusive, so the `+1` is required
      val randIdx = randombits.nextInt(currIdx+1)
      if (randIdx < k) sample(randIdx) = item
    }
    else sample(currIdx) = item
    
    currIdx += 1
    this
  }

  def addAll (items: Array[T]): ReservoirSampler[T] = {
    @tailrec
    def loop (i: Int): Unit =
      if (i >= items.length) Unit
      else {
	add(items(i))
	loop(i+1)
      }
    loop(0)
    this
  }

  def apply (i: Int): T = sample(i)

  def getSampleSet () = sample

  /** Number of elemtents in reservoir */
  def size () =
    if (currIdx >=k) k
    else currIdx
}

/** Simple TEMPORARY tests used for dev purposes */
object simpletests {
  val r = new Random()
  
  def randomTest (setSz: Int, lstSz: Int): AssociativeStreamSampler[Int] = {
    def loop (i: Int, accu: AssociativeStreamSampler[Int]):
    AssociativeStreamSampler[Int] = {
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
    println(randomTest(5, 15).getSampleSet().deep.mkString(" "))
    println(randomTest(5, 15).getSampleSet().deep.mkString(" "))
    println(randomTest(5, 15).getSampleSet().deep.mkString(" "))
    println(randomTest(5, 15).getSampleSet().deep.mkString(" "))
    println(randomTest(5, 15).getSampleSet().deep.mkString(" "))
    println(randomTest(5, 15).getSampleSet().deep.mkString(" "))
    println(randomTest(5, 15).getSampleSet().deep.mkString(" "))
    println(randomTest(5, 15).getSampleSet().deep.mkString(" "))
  }
}
