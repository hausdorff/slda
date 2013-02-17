/** Data structures and algorithms for processing streams. For example, this
 * package includes sampling algorithms, like reservoir sampling and heavy
 * hitters.
 */

package stream

import scala.annotation.tailrec

import scala.collection.mutable.Map
import scala.util.{ Random => Random }

/** Associative stream samplers are backed by a regular associative array,
 * meaning their elements are not key-addressable. Reservoir sampling, for
 * example, often produces an associative array of randomly sampled elements,
 * in random order.
 */
abstract class AssociativeStreamSampler[T] () {
  def add (item: T): AssociativeStreamSampler[T]
  def addAll (items: Array[T]): AssociativeStreamSampler[T]
  def size (): Int
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
  def size (): Int
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
  
  def size (): Int = sample.size
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
  def size (): Int = sample.size
}
