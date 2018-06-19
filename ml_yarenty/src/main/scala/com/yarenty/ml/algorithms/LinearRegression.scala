package com.yarenty.ml.algorithms

/**
  * Based on material from:
  * http://introcs.cs.princeton.edu/java/97data/LinearRegression.java.html
  * (C)2018 by yarenty
  */

class LinearRegression(Y: Array[Double]) {
  val n: Int = Y.length
  val res: Array[Double] = Array.ofDim[Double](Y.length)
  val X: Array[Double] = Array.ofDim[Double](Y.length)
  var k, b: Double = 0.0

  {

    for (i <- 0 until n) {
      X(i) = i + 1
    }
    // Calculate coefficients
    var sumx = 0.0
    var sumy = 0.0
    var sumx2 = 0.0
    // first pass: compute xbar and ybar
    for (i <- 0 until n) {
      sumx += X(i)
      sumx2 += X(i) * X(i)
      sumy += Y(i)
    }

    val xbar = sumx / n
    val ybar = sumy / n
    // second pass: compute summary statistics
    var xxbar = 0.0
    var yybar = 0.0
    var xybar = 0.0
    for (i <- 0 until n) {
      xxbar += (X(i) - xbar) * (X(i) - xbar)
      yybar += (Y(i) - ybar) * (Y(i) - ybar)
      xybar += (X(i) - xbar) * (Y(i) - ybar)
    }

    k = xybar / xxbar
    b = ybar - k * xbar

    // Calculate residuals
    for (i <- 0 until n) {
      res(i) = Y(i) - (k * X(i) + b)
    }
  }


  def coeff_k: Double = k
  def coeff_b: Double = b
  def residuals: Array[Double] = res

}

object LinearRegression {
  def apply(Y: Array[Double]): LinearRegression = {
    new LinearRegression(Y)
  }

  def error(Y: Array[Double], k: Double, b: Double): Array[Double] = {
    // given k and b calculate residuals on a new data
    val n = Y.length
    val r = Array.ofDim[Double](n)
    val X = Array.ofDim[Double](n)
    for (i <- 0 until n) {
      X(i) = i + 1

      r(i) = Y(i) - (k * X(i) + b)
    }
    r
  }
}
