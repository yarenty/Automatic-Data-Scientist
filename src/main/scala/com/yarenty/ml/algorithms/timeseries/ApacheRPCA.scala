package com.yarenty.ml.algorithms.timeseries

import org.apache.commons.math3.linear.{MatrixUtils, RealMatrix, SingularValueDecomposition}
import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics

/**
  * (C)2018 by yarenty
  */
class ApacheRPCA(val X: RealMatrix, val lpenalty: Double, val spenalty: Double) {

  val MAX_ITERS = 228
  private var L: RealMatrix = MatrixUtils.createRealMatrix(this.X.getRowDimension, this.X.getColumnDimension)
  private var S: RealMatrix = MatrixUtils.createRealMatrix(this.X.getRowDimension, this.X.getColumnDimension)
  private var E: RealMatrix = MatrixUtils.createRealMatrix(this.X.getRowDimension, this.X.getColumnDimension)

  computeRSVD()

  def this(data: Array[Array[Double]], lpenalty: Double, spenalty: Double) {
    this(MatrixUtils.createRealMatrix(data), lpenalty, spenalty)
  }


  def computeRSVD() {
    var mu = X.getColumnDimension * X.getRowDimension / (4 * l1norm(X.getData))
    var objPrev = 0.5 * Math.pow(X.getFrobeniusNorm, 2)
    var obj = objPrev
    val tol = 1e-8 * objPrev
    var diff = 2 * tol
    var iter = 0

    while (diff > tol && iter < MAX_ITERS) {
      val nuclearNorm = computeS(mu)
      val l1Norm = computeL(mu)
      val l2Norm = computeE()

      obj = computeObjective(nuclearNorm, l1Norm, l2Norm)
      diff = Math.abs(objPrev - obj)
      objPrev = obj

      mu = computeDynamicMu()

      iter = iter + 1
    }
  }

  private def l1norm(x: Array[Array[Double]]): Double = {
    var l1norm = 0.0
    for (i <- x.indices) {
      for (j <- x(i).indices) {
        l1norm += Math.abs(x(i)(j))
      }
    }
    l1norm
  }

  private def computeL(mu: Double): Double = {
    val LPenalty = lpenalty * mu
    val svd = new SingularValueDecomposition(X.subtract(S))
    val penalizedD = softThreshold(svd.getSingularValues, LPenalty)
    val D_matrix = MatrixUtils.createRealDiagonalMatrix(penalizedD)
    L = svd.getU.multiply(D_matrix).multiply(svd.getVT)
    penalizedD.sum * LPenalty
  }

  private def softThreshold(x: Array[Double], penalty: Double): Array[Double] = {
    for (i <- x.indices) {
      x(i) = Math.signum(x(i)) * Math.max(Math.abs(x(i)) - penalty, 0)
    }
    x
  }

  private def computeS(mu: Double): Double = {
    val SPenalty = spenalty * mu
    val penalizedS = softThreshold(X.subtract(L).getData, SPenalty)
    S = MatrixUtils.createRealMatrix(penalizedS)
    l1norm(penalizedS) * SPenalty
  }

  private def softThreshold(x: Array[Array[Double]], penalty: Double): Array[Array[Double]] = {
    for (i <- x.indices) {
      for (j <- x(i).indices) {
        x(i)(j) = Math.signum(x(i)(j)) * Math.max(Math.abs(x(i)(j)) - penalty, 0)
      }
    }
    x
  }

  private def computeE(): Double = {
    E = X.subtract(L).subtract(S)
    val norm = E.getFrobeniusNorm
    Math.pow(norm, 2)
  }

  private def computeObjective(nuclearnorm: Double, l1norm: Double, l2norm: Double): Double = {
    0.5 * l2norm + nuclearnorm + l1norm
  }

  private def computeDynamicMu(): Double = {
    val m = E.getRowDimension
    val n = E.getColumnDimension

    val E_sd = standardDeviation(E.getData)
    val mu = E_sd * Math.sqrt(2 * Math.max(m, n))

    Math.max(.01, mu)
  }

  /*private double MedianAbsoluteDeviation(double[][] x) {
    DescriptiveStatistics stats = new DescriptiveStatistics();
    for (int i = 0; i < x.length; i ++)
      for (int j = 0; j < x[i].length; j++)
        stats.addValue(x[i][j]);
    double median = stats.getPercentile(50);

    DescriptiveStatistics absoluteDeviationStats = new DescriptiveStatistics();
    for (int i = 0; i < x.length; i ++)
      for (int j = 0; j < x[i].length; j++)
        absoluteDeviationStats.addValue(Math.abs(x[i][j] - median));

    return absoluteDeviationStats.getPercentile(50) * 1.4826;
  }*/

  private def standardDeviation(x: Array[Array[Double]]): Double = {
    val stats = new DescriptiveStatistics()
    for (i <- x.indices)
      for (j <- x(i).indices)
        stats.addValue(x(i)(j))
    stats.getStandardDeviation
  }

  def getL: RealMatrix = L

  def getS: RealMatrix = S

  def getE: RealMatrix = E


}