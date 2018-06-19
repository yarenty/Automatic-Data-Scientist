package com.yarenty.ml.algorithms.timeseries

import org.apache.commons.math3.linear.{MatrixUtils, RealMatrix}

/**
  * (C)2018 by yarenty
  */
class AugmentedDickeyFuller(val ts: Array[Double], val lag: Int) {

  val PVALUE_THRESHOLD: Double = -3.45
  var needsDiff = true
  var zeroPaddedDiff: Array[Double] = _
  computeADFStatistics()

  /**
    * Uses the Augmented Dickey Fuller test to determine
    * if ts is a stationary time series
    */
  def this(ts: Array[Double]) {
    this(ts, Math.floor(Math.cbrt((ts.length - 1).toDouble)).toInt)
  }

  def isNeedsDiff: Boolean = needsDiff

  def getZeroPaddedDiff: Array[Double] = zeroPaddedDiff

  private def computeADFStatistics() = {
    val y = diff(ts)
    var designMatrix: RealMatrix = null
    val k = lag + 1
    val n = ts.length - 1

    val z = MatrixUtils.createRealMatrix(laggedMatrix(y, k)) //has rows length(ts) - 1 - k + 1
    val zcol1 = z.getColumnVector(0) //has length length(ts) - 1 - k + 1
    val xt1 = subsetArray(ts, k - 1, n - 1)
    //ts[k:(length(ts) - 1)], has length length(ts) - 1 - k + 1
    val trend = sequence(k, n); //trend k:n, has length length(ts) - 1 - k + 1
    if (k > 1) {
      val yt1 = z.getSubMatrix(0, ts.length - 1 - k, 1, k - 1) //same as z but skips first column
      //build design matrix as cbind(xt1, 1, trend, yt1)
      designMatrix = MatrixUtils.createRealMatrix(ts.length - 1 - k + 1, 3 + k - 1)
      designMatrix.setColumn(0, xt1)
      designMatrix.setColumn(1, ones(ts.length - 1 - k + 1))
      designMatrix.setColumn(2, trend)
      designMatrix.setSubMatrix(yt1.getData, 0, 3)

    } else {
      //build design matrix as cbind(xt1, 1, tt)
      designMatrix = MatrixUtils.createRealMatrix(ts.length - 1 - k + 1, 3)
      designMatrix.setColumn(0, xt1)
      designMatrix.setColumn(1, ones(ts.length - 1 - k + 1))
      designMatrix.setColumn(2, trend)
    }
    /*OLSMultipleLinearRegression regression = new OLSMultipleLinearRegression();
    regression.setNoIntercept(true);
    regression.newSampleData(zcol1.toArray(), designMatrix.getData());
    double[] beta = regression.estimateRegressionParameters();
    double[] sd = regression.estimateRegressionParametersStandardErrors();
    */
    val regression = new RidgeRegression(designMatrix.getData, zcol1.toArray)
    regression.updateCoefficients(.0001)
    val beta = regression.getCoefficients
    val sd = regression.getStandarderrors

    val t = beta(0) / sd(0)
    if (t <= PVALUE_THRESHOLD) {
      this.needsDiff = true
    } else {
      this.needsDiff = false
    }
  }

  /**
    * Takes finite differences of x
    *
    * @param x
    * @return Returns an array of length x.length-1 of
    *         the first differences of x
    */
  private def diff(x: Array[Double]): Array[Double] = {
    val diff = Array.ofDim[Double](x.length - 1)
    val zeroPaddedDiff = Array.ofDim[Double](x.length)

    for (i <- diff.indices) {
      val diff_i = x(i + 1) - x(i)
      diff(i) = diff_i
      zeroPaddedDiff(i + 1) = diff_i
    }
    this.zeroPaddedDiff = zeroPaddedDiff
    diff
  }

  /**
    * Equivalent to matlab and python ones
    *
    * @param n
    * @return an array of doubles of length n that are
    *         initialized to 1
    */
  private def ones(n: Int): Array[Double] = {

    Array.fill[Double](n)(1)

  }

  /**
    * Equivalent to R's embed function
    *
    * @param x   time series vector
    * @param lag number of lags, where lag=1 is the same as no lags
    * @return a matrix that has x.length - lag + 1 rows by lag columns.
    */
  private def laggedMatrix(x: Array[Double], lag: Int): Array[Array[Double]] = {
    val laggedMatrix = Array.ofDim[Double](x.length - lag + 1, lag)
    for (j <- 0 until lag) {
      //loop through columns
      for (i <- laggedMatrix.indices) {
        laggedMatrix(i)(j) = x(lag - j - 1 + i)
      }
    }
    laggedMatrix
  }

  /**
    * Takes x[start] through x[end - 1]
    *
    * @param x
    * @param start
    * @param end
    * @return
    */
  private def subsetArray(x: Array[Double], start: Int, end: Int): Array[Double] = {
    val subset = new Array[Double](end - start + 1)
    System.arraycopy(x, start, subset, 0, end - start + 1)
    subset
  }

  /**
    * Generates a sequence of ints [start, end]
    *
    * @param start
    * @param end
    * @return
    */
  private def sequence(start: Int, end: Int): Array[Double] = {
    val sequence = new Array[Double](end - start + 1)
    for (i <- start to end) {
      sequence(i - start) = i
    }
    sequence
  }
}

