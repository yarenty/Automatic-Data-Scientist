package com.yarenty.ml.data.transformations.movingaverage

class ExponentialMovingAverage(var alpha: Double) extends MovingAverage {
  private var oldValue: Double = Double.NaN

  def average(data: Array[Double]): Array[Double] = {
    data.map(avg)
  }

  def avg(value: Double): Double = {
    if (oldValue.isNaN) {
      oldValue = value
      value
    } else {
      val newValue = oldValue + alpha * (value - oldValue)
      oldValue = newValue
      newValue
    }
  }
}