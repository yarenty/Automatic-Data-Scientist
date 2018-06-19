package com.yarenty.ml.data.transformations.movingaverage

import scala.collection.mutable

class SimpleMovingAverage(val period: Int) extends MovingAverage {
  assert(period > 0, "Period must be a positive integer!")

  val window = new mutable.Queue[Double]()
  private var sum = .0

  def average(data: Array[Double]): Array[Double] = {
    val ma_data = Array.ofDim[Double](data.length)

    for (x <- data.indices) {
      newNum(data(x))
      ma_data(x) = getAvg
    }
    ma_data
  }

  def newNum(num: Double): Unit = {
    sum += num
    window += num
    if (window.size > period) sum -= window.dequeue
  }

  def getAvg: Double = {
    if (window.isEmpty) {
      0.0 // technically the average is undefined
    } else {
      sum / window.size
    }
  }
}