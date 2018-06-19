package com.yarenty.ml.data.transformations.smoothing.h2oframe

import org.apache.spark.h2o.Frame

trait Smoothing {
  def forecast(data: Frame): Array[Double]
}
