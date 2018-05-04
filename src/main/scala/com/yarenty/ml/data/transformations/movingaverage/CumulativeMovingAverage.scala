package com.yarenty.ml.data.transformations.movingaverage

class CumulativeMovingAverage extends MovingAverage {
  var n = 0
  var average = 0.0

  def average(data: Array[Double]): Array[Double] = {
    val out = Array.ofDim[Double](data.length)
    for (i <- data.indices) out(i) = add(data(i))
    out
  }

  def add(x: Double): Double = {
    n += 1
    average += (x - average) / n
    average
  }
}