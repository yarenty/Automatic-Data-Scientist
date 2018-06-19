package com.yarenty.math

import org.apache.commons.math3.random.EmpiricalDistribution
import org.apache.commons.math3.stat.descriptive.SummaryStatistics
import org.apache.commons.math3.stat.descriptive.moment.StandardDeviation

class Histogram(data: Array[Double]) {

  lazy val getBinWidth: Double = binWidth
  private var binWidth = 0.0

  /**
    * Default uses Scott's rule - so make sure your data is Gaussian or Gaussian like!!
    * If not - decide about binWidth by yourself!
    *
    * @return
    */
  def default: Array[Int] = {
    binWidth = getScottsBinWidth
    val numBins = ((data.max - data.min) / binWidth).toInt

    val result = Array.ofDim[Int](numBins + 1)

    for (d <- data) {
      val bin = ((d - data.min) / binWidth).toInt

      if (bin >= 0 && bin < numBins + 1) {
        result(bin) = result(bin) + 1
      }
    }
    result
  }

  /**
    * Calculate number of bins for historgrams - Scott's rule!
    *
    * @return
    */
  def getScottsBinWidth: Double = {
    val std = new StandardDeviation().evaluate(data)
    if (std.isInfinity || std.isNaN) {
      data.length / 10
    } else {
      val out = (3.5 * std) / Math.pow(data.length, 1.0 / 3.0)
      out
    }
  }

  def byNumberOfBins(binCount: Int): Array[Int] = {
    binWidth = (data.max - data.min) / (binCount - 1)
    val result = Array.ofDim[Int](binCount)
    for (d <- data) {
      val bin = ((d - data.min) / binWidth).toInt

      if (bin >= 0 && bin < binCount + 1) {
        result(bin) = result(bin) + 1
      }
    }
    result
  }

  def byBinWidth(binWidth: Double): Array[Int] = {
    this.binWidth = binWidth
    val numBins = ((data.max - data.min) / binWidth).toInt

    val result = Array.ofDim[Int](numBins + 1)

    for (d <- data) {
      val bin = ((d - data.min) / binWidth).toInt

      if (bin >= 0 && bin < numBins + 1) {
        result(bin) = result(bin) + 1
      }
    }
    result
  }

  def empirical(binCount: Int): Array[Int] = {
    val distribution = new EmpiricalDistribution(binCount)
    binWidth = (data.max - data.min) / (binCount - 1)
    distribution.load(data)
    val histogram = Array.ofDim[Int](distribution.getBinCount)
    val v = distribution.getBinStats.toArray
    var i = 0
    import collection.JavaConversions._
    for (stats: SummaryStatistics <- distribution.getBinStats) {
      histogram(i) = stats.getN.toInt
      i = i + 1
    }
    histogram
  }
}
