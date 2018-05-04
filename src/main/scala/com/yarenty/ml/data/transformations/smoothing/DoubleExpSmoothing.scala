package com.yarenty.ml.data.transformations.smoothing


class DoubleExpSmoothing(alpha: Double, gamma: Double, initializationMethod: InitializationMethod, numForecasts: Int) extends Smoothing {


  def forecast(data: List[Double]): Array[Double] = {
    assert(data.length > 4, "Not enough data to do smoothing")

    val y = Array.ofDim[Double](data.size + numForecasts)
    val s = Array.ofDim[Double](data.size)
    val b = Array.ofDim[Double](data.size)
    s(0) = data(0)
    y(0) = s(0)

    initializationMethod match {
      case Diff => b(0) = data(1) - data(0)
      case Avg4 => b(0) = (data(3) - data(0)) / 3
      case AvgAll => b(0) = (data(data.size - 1) - data(0)) / (data.size - 1)
    }
    //    var i = 1
    y(1) = s(0) + b(0)
    for (i <- 1 until data.size) {
      s(i) = alpha * data(i) + (1 - alpha) * (s(i - 1) + b(i - 1))
      b(i) = gamma * (s(i) - s(i - 1)) + (1 - gamma) * b(i - 1)
      y(i + 1) = s(i) + b(i)
    }

    for (i <- 0 until numForecasts) {
      y(data.size + i) = s(data.size - 1) + (i + 1) * b(data.size - 1)
    }
    y
  }

}
