package com.yarenty.ml.data.transformations.smoothing.h2oframe

import com.yarenty.ml.data.transformations.smoothing.{Avg4, AvgAll, Diff, InitializationMethod}
import water.fvec.Frame

class DoubleExpSmoothing(alpha: Double, gamma: Double, initializationMethod: InitializationMethod, numForecasts: Int) extends Smoothing {


  def forecast(data: Frame): Array[Double] = {
    var y = new Array[Double](0)
    for (v <- data.vecs) { //Initialize vec length
      val n = v.length.toInt
      assert(n > 4, "Not enough data to do smoothing")
      y = new Array[Double](n + numForecasts)
      val s = new Array[Double](n)
      val b = new Array[Double](n)

      s(0) = v.at(0)
      y(0) = s(0)

      initializationMethod match {
        case Diff => b(0) = v.at(1) - v.at(0)
        case Avg4 => b(0) = (v.at(3) - v.at(0)) / 3
        case AvgAll => b(0) = (v.at(n - 1) - v.at(0)) / (n - 1)
      }


      y(1) = s(0) + b(0)
      for (i <- 1 until n) {
        s(i) = alpha * v.at(i) + (1 - alpha) * (s(i - 1) + b(i - 1))
        b(i) = gamma * (s(i) - s(i - 1)) + (1 - gamma) * b(i - 1)
        y(i + 1) = s(i) + b(i)
      }
      for (j <- 0 until numForecasts) {
        y(j + n) = s(n - 1) + (j + 1) * b(n - 1)
      }
    }
    y
  }

}
