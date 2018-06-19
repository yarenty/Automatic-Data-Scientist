package com.yarenty.math.correlation

import org.apache.commons.math3.stat.correlation.KendallsCorrelation

case object Kendalls extends Correlation {
  override def correlate(expected: Array[Double], observed: Array[Double]): Double = {
    checks(expected, observed)
    new KendallsCorrelation().correlation(expected, observed)
  }
}
