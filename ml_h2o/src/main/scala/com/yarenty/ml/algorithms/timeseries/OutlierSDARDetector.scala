package com.yarenty.ml.algorithms.timeseries

import scala.collection.mutable.ArrayBuffer

/**
  * Outlier and Changepoint detector  using SDAR.
  *
  * C(C)2018 by yarenty
  */

trait LossFunction

case object LogLoss extends LossFunction

case object Hellinger extends LossFunction

class OutlierSDARDetector(sdar_k: Int, sdar_x_forget: Int, sdar_y_forget: Int, sdar_x_window: Int, sdar_y_window: Int,
                          sdar_lossFunc1: LossFunction, sdar_lossFunc2: LossFunction) {
  assert(sdar_k >= 2, "k must be greater than 1: " + sdar_k)
  assert(sdar_x_forget > 0.0d && sdar_x_forget < 1.0d, "x_forget must be in range (0,1): " + sdar_x_forget)
  assert(sdar_y_forget > 0.0d && sdar_y_forget < 1.0d, "y_forget must be in range (0,1): " + sdar_y_forget)
  assert(sdar_x_window >= 2, "x window must be greather than 1: " + sdar_x_window)
  assert(sdar_y_window >= 2, "y window must be greather than 1: " + sdar_y_window)

  val sdar1 = new SequentiallyDiscountingAutoRegression(sdar_x_forget, sdar_k)
  val sdar2 = new SequentiallyDiscountingAutoRegression(sdar_y_forget, sdar_k)
  val xRing = new ArrayBuffer[Double](sdar_k + 1)
  val yRing = new ArrayBuffer[Double](sdar_k + 1)
  val outlierScores = new ArrayBuffer[Double](sdar_x_window)
  val changepointScores = new ArrayBuffer[Double](sdar_y_window)
  var xSeries = new Array[Double](sdar_k + 1)
  var ySeries = new Array[Double](sdar_k + 1)

  def calculate(x: Double, outScores: Array[Double]): Unit = {
    // [Stage#1] Outlier Detection
    xRing += x
    if (xRing.size > sdar_k + 1) xRing.remove(0)
    xSeries = xRing.reverse.toArray
    val k1 = xRing.size - 1

    val x_hat = sdar1.update(xSeries, k1)
    val scoreX = if (k1 == 0) 0.0d else loss(sdar1, x, x_hat, sdar_lossFunc1)

    // smoothing
    outlierScores += scoreX
    if (outlierScores.size > sdar_k) outlierScores.remove(0)
    val y = outlierScores.sum / outlierScores.size

    // [Stage#2] Change-point Detection
    yRing += y
    if (yRing.size > sdar_k + 1) yRing.remove(0)
    ySeries = yRing.reverse.toArray

    val k2 = yRing.length - 1
    val y_hat = sdar2.update(ySeries, k2)
    val lossY = if (k2 == 0) 0.0d else loss(sdar2, y, y_hat, sdar_lossFunc2)

    changepointScores += lossY
    if (changepointScores.size > sdar_k) changepointScores.remove(0)
    val scoreY = changepointScores.sum / changepointScores.size //smoothing

    outScores(0) = scoreX
    outScores(1) = scoreY
  }

  private def loss(sdar: SequentiallyDiscountingAutoRegression, actual: Double, predicted: Double, lossFunc: LossFunction) = {
    var loss = .0
    lossFunc match {
      case Hellinger =>
        loss = sdar.hellingerDistance * 100.0d
      case LogLoss =>
        loss = sdar.logLoss(actual, predicted)
    }
    loss
  }

}
