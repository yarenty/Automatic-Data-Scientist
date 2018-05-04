package com.yarenty.ml.data.correlation

import com.yarenty.ml.data.distribution.Histogram
import org.apache.commons.math3.stat.inference.ChiSquareTest
import water.util.Log

/**
  * Use ChiSquareTest over distribution of both expected and observed bins
  * while generally ChiSquareTest should be over frequencies of both expected and observed bins
  */
case object ChiSquareCorrelation extends Correlation {
  override def correlate(expected: Array[Double], observed: Array[Double]): Double = {
    correlate(expected, observed, true)
  }

  //chisqure works on true positives - expacted cannot be 0 at any point
  def correlate(expected: Array[Double], observed: Array[Double], secure: Boolean = true): Double = {
    checks(expected, observed)
    val (exp, obs) = calculateBins(expected, observed)
    Log.debug("EXPECTED:" + exp.mkString(", "))
    Log.debug("OBSERVED:" + obs.mkString(", "))
    val out = if (secure) {
      new ChiSquareTest().chiSquareTest(exp.map(x => (x + 1).toDouble), obs.map(x => (x + 1).toLong))
    } else {
      new ChiSquareTest().chiSquareTest(exp.map(_.toDouble), obs.map(_.toLong))
    }
    println("ChiSquared out: " + out)
    out
  }

  def calculateBins(expected: Array[Double], observed: Array[Double]): (Array[Int], Array[Int]) = {
    val h1 = new Histogram(expected).default
    val h2 = new Histogram(observed).byNumberOfBins(h1.length)
    (h1, h2)
  }

  //temp: correlation using histograms - distribution of input data!
  override def correlated(expected: Array[Double], observed: Array[Double]): Boolean = {
    checks(expected, observed)
    val (exp, obs) = calculateBins(expected, observed)
    try {
      !new ChiSquareTest().chiSquareTest(exp.map(_.toDouble), obs.map(_.toLong), 0.1)
    } catch {
      case e: Exception =>
        Log.warn(e)
        false
    }
  }

}
