/** Data structures and algorithms for processing streams. For example, this
 * package includes sampling algorithms, like reservoir sampling and heavy
 * hitters.
 */

package stream

import scala.annotation.tailrec
import scala.collection.mutable.Map
import scala.util.{ Random => Random }

import globals.Constants

/** Associative stream samplers are backed by a regular associative array,
 * meaning their elements are not key-addressable. Reservoir sampling, for
 * example, often produces an associative array of randomly sampled elements,
 * in random order.
 */
abstract class AssociativeStreamSampler[T] () {
  def add (item: T): AssociativeStreamSampler[T]
  def addAll (items: Array[T]): AssociativeStreamSampler[T]
  def capacity (): Int // total number of slots availble in sampler
  def occupied (): Int // total number of slots occupied in sampler
  def apply (i: Int): T
  def getSampleSet (): Array[T]
}

/** Mapping stream samplers are backed by a map or hash table, and thus their
 * elements are key-addressable. The Misra-Gries approach to the heavy hitters
 * problem, for example, will return a map of elements that are believed to have
 * occurred more than k times.
 */
abstract class MappingStreamSampler[T] () {
  def add (item: T): MappingStreamSampler[T]
  def addAll (items: Array[T]): MappingStreamSampler[T]
  def capacity (): Int // total number of slots availble in sampler
  def occupied (): Int // total number of slots occupied in sampler
  def apply (item: T): Int
  def getSampleSet (): Map[T, Int]
}

/** Simple implementation of reservoir sampling.
 *
 * The classical treatment is given by Vitter in Random Sampling With a
 * Reservoir, but it's very well known at this point.
 *
 * WARNING: HIGHLY STATEFUL
 */
class ReservoirSampler[T: Manifest] (k: Int) extends
AssociativeStreamSampler[T] () {
  var sample = new Array[T](k)
  var currIdx = 0
  var randombits = new Random()

  /** Add returns a ReservoirSampler so we can chain `add` calls together */
  def add (item: T): ReservoirSampler[T] = {
    addItem(item)
    this
  }

  /** Add returns index of its place in reservoir, returns
   `DidNotAddToSampler` if we did not place it in the reservoir. */
  def addItem (item: T): Int = {
    var slotToReplace = Constants.DidNotAddToSampler
    if (currIdx >= k) {
      // IMPORTANT: `nextInt()` not inclusive, so the `+1` is required
      slotToReplace = randombits.nextInt(currIdx+1)
      if (slotToReplace < k) sample(slotToReplace) = item
      else slotToReplace = Constants.DidNotAddToSampler
    }
      else {
	sample(currIdx) = item
	slotToReplace = currIdx
      }
    
    currIdx += 1
    slotToReplace
  }

  def addAll (items: Array[T]): ReservoirSampler[T] = {
    items.foreach { item => add(item) }
    this
  }

  def apply (i: Int): T = {
    if (i >= currIdx)
      throw new RuntimeException("reservoir sample hasn't seen " + i +
				 " objects yet!")
    else sample(i)
  }

  /** Output an array with all the elements in the sample; ie, if our sample
   has < k elements in it, we only output the elements we have */
  def getSampleSet (): Array[T] = {
    if (currIdx < k) {
      var out = new Array[T](currIdx)
      Array.copy(sample, 0, out, 0, currIdx)
      return out
    }
    else sample
  }

  /** Capacity of sampler, ie, maximum number of slots available total */
  def capacity () = k

  /** Number of elemtents in reservoir */
  def occupied () =
    if (currIdx >= k) k
    else currIdx

  override def toString(): String = getSampleSet.deep.mkString(" ")
}

/** Implements the Misra-Gries Frequent algorithm for the heavy hitters problem.
 *
 * For details see "Space-optimal Heavy Hitters with Strong Error Bounds",
 * by Berinde, Indyk, Cormode, and Strauss.
 *
 * WARNING: HIGHLY STATEFUL
 */
class FrequentSampler[T: Manifest] (k: Int) extends
MappingStreamSampler[T] () {
  var sample = Map[T, Int]()
  
  def add (item: T): FrequentSampler[T] = {
    if (sample.contains(item))
      sample += item -> (sample(item) + 1)
    else if (sample.size < k)
      sample += item -> 1
    else {
      for ((k,v) <- sample) {
	if (v == 1) sample -= k
	else sample += k -> (v - 1)
      }
    }

    this
  }
  
  def addAll (items: Array[T]): FrequentSampler[T] = {
    @tailrec
    def loop (i: Int): Unit = {
      if (i >= items.length) Unit
      else {
	add(items(i))
	loop(i+1)
      }
    }
    loop(0)
    this
  }
  
  def apply (item: T): Int =
    if (sample contains item) sample(item)
    else 0
  
  def getSampleSet (): Map[T, Int] = sample
  
  def capacity (): Int = k
  def occupied (): Int = sample.size
}

/** Implements the SpaceSaving algorithm for the heavy hitters problem.
 *
 * For details see "Space-optimal Heavy Hitters with Strong Error Bounds",
 * by Berinde, Indyk, Cormode, and Strauss.
 *
 * WARNING: HIGHLY STATEFUL
 */
class SpaceSavingSampler[T: Manifest] (k: Int) extends
MappingStreamSampler[T] () {
  var sample = Map[T, Int]()
  
  def add (item: T): SpaceSavingSampler[T] = {
    if (sample.contains(item))
      sample += item -> (sample(item) + 1)
    else if (sample.size < k)
      sample += item -> 1
    else {
      val y = sample.minBy(_._2)._1
      sample += item -> (sample(y) + 1)
      sample -= y
    }

    this
  }
  
  def addAll (items: Array[T]): SpaceSavingSampler[T] = {
    @tailrec
    def loop (i: Int): Unit = {
      if (i >= items.length) Unit
      else {
	add(items(i))
	loop(i+1)
      }
    }
    loop(0)
    this
  }
  
  def apply (item: T): Int =
    if (sample contains item) sample(item)
    else 0
  
  def getSampleSet (): Map[T, Int] = sample
  def capacity (): Int = k
  def occupied (): Int = sample.size
}
