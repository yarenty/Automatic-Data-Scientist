package com.yarenty.math.correlation


/**
  * The Correlation Coefficient has several advantages over covariance for determining strengths of relationships:
  *
  * Covariance can take on practically any number while a correlation is limited: -1 to +1.
  * Because of it’s numerical limitations, correlation is more useful for determining how strong the relationship is between the two variables.
  * Correlation does not have units. Covariance always has units
  * Correlation isn’t affected by changes in the center (i.e. mean) or scale of the variables
  *
  * The population correlation coefficient uses σx and σy as the population standard deviations, and σxy as the population covariance.
  *
  * (C)2018 by yarenty
  */
case object CoefficientCorrelation extends Correlation {

  override def correlate(expected: Array[Double], observed: Array[Double]): Double = {
    checks(expected, observed)
    var corr = 0.0
    var mean1 = 0.0
    var mean2 = 0.0
    var mag1 = 0.0
    var mag2 = 0.0

    for (i <- expected.indices) {
      mean1 += expected(i)
      mean2 += observed(i)
    }
    mean1 /= expected.length
    mean2 /= expected.length

    val cexp = expected.clone
    val cobs = observed.clone
    for (i <- expected.indices) {
      cexp(i) -= mean1
      cobs(i) -= mean2
      mag1 += cexp(i) * cexp(i)
      mag2 += cobs(i) * cobs(i)
      corr += cobs(i) * cexp(i)

    }
    corr /= Math.sqrt(mag1 * mag2)

    corr
  }
}