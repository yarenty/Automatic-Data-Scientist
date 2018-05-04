package com.yarenty.ml.data.transformations.smoothing.h2oframe

import water.fvec.Frame

class SingleExpSmoothing(alpha: Double, numForecasts: Int) extends Smoothing {


  def forecast(data: Frame): Array[Double] = {
    var y = new Array[Double](0)
    for (v <- data.vecs) {
      val n = v.length.toInt
      y = new Array[Double](n + numForecasts)
      //init
      y(0) = 0
      y(1) = v.at(0)

      //smoothing
      for (i <- 2 until n) {
        y(i) = alpha * v.at(i - 1) + (1 - alpha) * y(i - 1)
      }

      //forecast
      for (i <- n until n + numForecasts) {
        y(i) = alpha * v.at(v.length - 1) + (1 - alpha) * y(i - 1)
      }
    }

    y
  }
}
