package com.yarenty.ml.data.correlation

import org.apache.commons.math3.stat.correlation.SpearmansCorrelation
import water.util.Log

case object Spearmans extends Correlation {
  override def correlate(expected: Array[Double], observed: Array[Double]): Double = {
    checks(expected, observed)
    try {
      new SpearmansCorrelation().correlation(expected, observed)
    } catch {
      case e: Exception =>
        Log.warn(e)
        Double.NaN
    }
  }
}
