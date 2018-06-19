package com.yarenty.math.correlation

import scala.collection.mutable.ListBuffer
//import water.util.Log

/**
  * (C)2018 by yarenty
  */
trait Correlation {
  def removeNANs(expected: Array[Double], observed: Array[Double]): (Array[Double], Array[Double]) = {
    val ex = ListBuffer[Double]()
    val ob = ListBuffer[Double]()
    if (!expected.isEmpty) {
      for (i <- expected.indices) {
        if (!expected(i).isNaN && !observed(i).isNaN) {
          ex += expected(i)
          ob += observed(i)
        }
      }
      (ex.toArray, ob.toList.toArray)
    } else {
      (expected, observed)
    }
  }


  def correlate(expected: Array[Double], observed: Array[Double]): Double = {
    checks(expected, observed)
    Double.NaN
  }

  def checks(expected: Array[Double], observed: Array[Double]): Unit = {
    assert(expected == null || !expected.isEmpty, "Cannot correlate: Empty objects")
    assert(observed == null || !observed.isEmpty, "Cannot correlate: Empty observations")
    assert(expected.length == observed.length, "Cannot correlate: Dimensions mismatch")
  }

  def correlated(expected: Array[Double], observed: Array[Double]): Boolean = false

}

case object MLCorrelation extends Correlation

case object REACorrelation extends Correlation

case object ReliefFCorrelation extends Correlation













