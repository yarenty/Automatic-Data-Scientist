package com.yarenty.ml.data.transformations.movingaverage.h2oframe

import water.fvec.Frame

import scala.collection.mutable

/**
  * Simple Moving Average (SMA)
  *
  * A simple moving average (SMA) is the unweighted mean of the previous n data. However, in science and engineering
  * the mean is normally taken from an equal number of data on either side of a central value. This ensures that
  * variations in the mean are aligned with the variations in the data rather than being shifted in time.
  * An example of a simple equally weighted running mean for a n-day sample of closing price is the mean of
  * the previous n days' closing prices.
  *
  * (C)2018 by yarenty
  */
class SimpleMovingAverage(val period: Int) extends MovingAverage {
  assert(period > 0, "Period must be a positive integer!")

  val window = new mutable.Queue[Double]()
  private var sum = .0


  def average(data: Frame): Frame = {
    for (v <- data.vecs) {
      for (x <- 0 until v.length.toInt) {
        newNum(v.at(x))
        v.set(x, getAvg)
      }
    }
    data
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