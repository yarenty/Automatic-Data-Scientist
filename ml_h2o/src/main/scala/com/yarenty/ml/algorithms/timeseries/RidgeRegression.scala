package com.yarenty.ml.algorithms.timeseries

import org.apache.commons.math3.linear.{MatrixUtils, RealMatrix, SingularValueDecomposition}

/**
  * (C)2018 by yarenty
  */
class RidgeRegression(val X: RealMatrix, val Y: Array[Double]) {

  //private val X = MatrixUtils.createRealMatrix(x)
  private val X_svd = new SingularValueDecomposition(X)
  private val residuals: Array[Double] = Array.ofDim[Double](Y.length)
  private var l2penalty = 0.0
  private var coefficients: Array[Double] = _
  private var standarderrors: Array[Double] = _
  private var fitted: Array[Double] = Array.ofDim[Double](Y.length)

  def this(x: Array[Array[Double]], Y: Array[Double]) {
    this(MatrixUtils.createRealMatrix(x), Y)
  }


  def updateCoefficients(l2penalty: Double) {

    val V = this.X_svd.getV
    val s = this.X_svd.getSingularValues
    val U = this.X_svd.getU

    for (i <- 0 until s.length) {
      s(i) = s(i) / (s(i) * s(i) + l2penalty)
    }
    val S = MatrixUtils.createRealDiagonalMatrix(s)

    val Z = V.multiply(S).multiply(U.transpose())

    this.coefficients = Z.operate(this.Y)

    this.fitted = this.X.operate(this.coefficients)
    var errorVariance = 0.0
    for (i <- residuals.indices) {
      this.residuals(i) = this.Y(i) - this.fitted(i)
      errorVariance += this.residuals(i) * this.residuals(i)
    }
    errorVariance = errorVariance / (X.getRowDimension - X.getColumnDimension)

    val errorVarianceMatrix = MatrixUtils.createRealIdentityMatrix(this.Y.length).scalarMultiply(errorVariance)
    val coefficientsCovarianceMatrix = Z.multiply(errorVarianceMatrix).multiply(Z.transpose())
    this.standarderrors = getDiagonal(coefficientsCovarianceMatrix)
  }

  private def getDiagonal(X: RealMatrix): Array[Double] = {
    val diag = Array.ofDim[Double](X.getColumnDimension)
    for (i <- diag.indices) {
      diag(i) = X.getEntry(i, i)
    }
    diag
  }

  def getL2penalty: Double = l2penalty

  def setL2penalty(l2pen: Double): Unit = {
    this.l2penalty = l2pen
  }

  def getCoefficients: Array[Double] = coefficients

  def getStandarderrors: Array[Double] = standarderrors

}