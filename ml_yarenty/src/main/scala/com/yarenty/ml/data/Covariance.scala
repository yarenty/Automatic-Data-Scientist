package com.yarenty.ml.data

/**
  * (C)2018 by yarenty
  *
  * @param expected
  * @param observed
  */
class Covariance(expected: Array[Double], observed: Array[Double]) {
  assert(expected == null || !expected.isEmpty, "Cannot correlate: Empty objects")
  assert(observed == null || !observed.isEmpty, "Cannot correlate: Empty observations")
  assert(expected.length == observed.length, "Cannot correlate: Dimensions mismatch")

  def covariance: Double = {
    new org.apache.commons.math3.stat.correlation.Covariance().covariance(expected, observed)
  }

}

case class StorelessCovariance(expected: Array[Double], observed: Array[Double]) extends Covariance(expected, observed) {
  override def covariance: Double = {
    new org.apache.commons.math3.stat.correlation.StorelessCovariance(4).covariance(expected, observed)
  }
}
