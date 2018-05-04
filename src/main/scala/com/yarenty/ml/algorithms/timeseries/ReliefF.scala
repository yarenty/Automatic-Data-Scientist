package com.yarenty.ml.algorithms.timeseries

import water.fvec.{Frame, Vec}
import water.util.Log

import scala.collection.immutable.HashMap


/**
  * (C)2018 by yarenty
  */
class ReliefF(val data: Frame, toVote: Array[String], exempt: Array[String]) {

  var numNeighbors: Int = 4
  var nearestHit: Vector[String] = Vector.empty[String]
  var nearestMiss: Vector[String] = Vector.empty[String]

  /**
    * Implementation of the RELIEF attribute evaluation algorithm.
    *
    * This implementation is extended to include more neighbors in calculating the
    * weights of the features.
    */

  def build(): Map[String, Double] = {
    Log.info(s"ReliefF: INPUT(${toVote.length}): ${toVote.mkString("; ")}")
    numNeighbors = toVote.length / 2
    var weights: Map[String, (Double, Array[String])] = new HashMap[String, (Double, Array[String])]()
    if (numNeighbors < 3) {
      toVote.map((_, 1.0)).toMap
    } else {
      for (m <- toVote) {
        findNearest(m)
        var d = diff(m, nearestHit)
        //remove by distance
        //        var c = nearestHit.length
        //        while (d > 0.1  && c > 1) {
        //          removeFarthest(nearestHit, m)
        //          d = diff(m, nearestHit)
        //          c = nearestHit.length
        //        }

        weights = weights + (m -> (d, nearestHit.toArray))
      }

      Log.info(s"ReliefF: processing WEIGHTS(${weights.size})")

      var out = Array.empty[String]
      var toRemove = HashMap.empty[String, Int]

      for (t <- toVote) {
        if (!toRemove.contains(t) || (toRemove.contains(t) && toRemove(t) < 2)) // do not remove topX
          out = out ++ Array(t)
        Log.info(s"ReliefF: $t -> ${weights(t)._1}  => ${weights(t)._2.mkString(",")}")
        for (w <- weights(t)._2) {
          if (toRemove.contains(w)) {
            toRemove = toRemove + (w -> (toRemove(w) + 1))
          } else {
            toRemove = toRemove + (w -> 1)
          }
        }
      }

      Log.info(s"STATS[$numNeighbors]::${toRemove.mkString("; ")}")
      exempt.foreach({ t => if (!out.contains(t)) out = out :+ t }) //always get topX GBM

      Log.info(s"Voting: OUTPUT(${out.length}): ${out.mkString("; ")}")
      out.map(k => (k, weights(k)._1)).toMap
    }
  }

  def diff(a: String, vector: Vector[String]): Double = {
    var sum = 0.0
    val A = data.vec(a)
    for (b <- vector) {
      val B = data.vec(b)
      for (i <- 0L until A.length)
        sum += Math.abs(A.at(i) - B.at(i))
    }
    sum / A.length / vector.length
  }

  /*
   * Find nearest neighbors that have the same class and that have another
   * class value. The results are stored in the vectors nearestHit and
   * nearestMiss.
   */
  def findNearest(processed: String) {
    nearestHit = Vector[String]()
    for (i <- toVote) {
      if (i != processed) {
        nearestHit = nearestHit :+ i
        if (nearestHit.length > numNeighbors) removeFarthest(nearestHit, processed)
        //        //remove by distance
        //        if (nearestHit.length>2 && diff(processed, nearestHit) > 0.1) removeFarthest(nearestHit, processed)
      }
    }
  }

  /*
   * Removes the element from the vector that is farthest from the supplied
   * element.
   */
  def removeFarthest(vector: Vector[String], supplied: String): Unit = {
    var tmp = 0
    var tmpS = ""
    var max = 0.0
    for (i <- vector.indices) {
      val inst = vector(i)
      val tmpDist = measure(data.vec(inst), data.vec(supplied))
      if (tmpDist > max) {
        max = tmpDist
        tmp = i
        tmpS = inst
      }
    }
    //      if (max>0.0) nearestHit = vector.drop(tmp)
    if (max > 0.0) nearestHit = vector.take(tmp) ++ vector.drop(tmp + 1)
    //      nearestHit = vector.drop(tmp)
  }

  /**
    * Calculates the Manhattan distance as the sum of the absolute differences
    * of their coordinates.
    *
    * @return the Manhattan distance between the two instances.
    */
  def measure(x: Vec, y: Vec): Double = {
    var sum = 0.0
    for (i <- 0L until x.length) {
      sum += Math.abs(x.at(i) - y.at(i))
    }
    sum
  }

}
