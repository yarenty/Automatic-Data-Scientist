package com.yarenty.ml.data.correlation

import org.apache.commons.math3.stat.correlation.PearsonsCorrelation

case object Pearsons extends Correlation {
  override def correlate(expected: Array[Double], observed: Array[Double]): Double = {
    checks(expected, observed)
    val (ex, ob) = removeNANs(expected, observed)
    new PearsonsCorrelation().correlation(ex, ob)
  }
}