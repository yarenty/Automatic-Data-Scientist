package com.yarenty.ml.data.transformations.movingaverage.h2oframe

import water.fvec.Frame


/**
  * Cumulative Moving Average (CMA)
  *
  * In a cumulative moving average, the data arrive in an ordered datum stream, and the user would like to get the
  * average of all of the data up until the current datum point. For example, an investor may want the average price
  * of all of the stock transactions for a particular stock up until the current time. As each new transaction occurs,
  * the average price at the time of the transaction can be calculated for all of the transactions up to that point using
  * the cumulative average, typically an equally weighted average of the sequence of n values x_1...x_n up to the
  * current time.
  *
  * (C)2018 by yarenty
  */

class CumulativeMovingAverage extends MovingAverage {
  var n = 0
  var average = 0.0

  def average(data: Frame): Frame = {
    for (v <- data.vecs) {
      for (x <- 0 until v.length.toInt) {
        v.set(x, add(v.at(x)))
      }
    }
    data
  }

  def add(x: Double): Double = {
    n += 1
    average += (x - average) / n
    average
  }
}