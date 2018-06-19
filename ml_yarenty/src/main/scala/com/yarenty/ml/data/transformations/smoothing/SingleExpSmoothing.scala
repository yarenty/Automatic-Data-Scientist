package com.yarenty.ml.data.transformations.smoothing

class SingleExpSmoothing(alpha: Double, numForecasts: Int) extends Smoothing {

  def forecast(data: List[Double]): Array[Double] = {
    val y = Array.ofDim[Double](data.size + numForecasts)
    //initial values
    y(0) = 0
    y(1) = data(0)
    //smoothing
    for (i <- 2 until data.size) y(i) = alpha * data(i - 1) + (1 - alpha) * y(i - 1)
    //predictions
    for (i <- data.size until data.size + numForecasts) y(i) = alpha * data(data.size - 1) + (1 - alpha) * y(i - 1)
    y
  }

}
