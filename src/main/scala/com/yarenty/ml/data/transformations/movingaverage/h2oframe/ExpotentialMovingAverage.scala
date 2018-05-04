package com.yarenty.ml.data.transformations.movingaverage.h2oframe

import water.fvec.Frame

/**
  * Exponential Moving Average (EMA)
  *
  * An exponential moving average (EMA), also known as an exponentially weighted moving average (EWMA), is a
  * type of infinite impulse response filter that applies weighting factors which decrease exponentially.
  * The weighting for each older datum decreases exponentially, never reaching zero.
  *
  * (C)2018 by yarenty
  */
class ExponentialMovingAverage(var alpha: Double) extends MovingAverage {
  private var oldValue: Double = Double.NaN

  def average(data: Frame): Frame = {
    for (v <- data.vecs) {
      for (x <- 0 until v.length.toInt) {
        v.set(x, avg(v.at(x)))
      }
    }
    data
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