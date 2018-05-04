package com.yarenty.ml.data.transformations.smoothing

trait InitializationMethod

case object Diff extends InitializationMethod

case object Avg4 extends InitializationMethod

case object AvgAll extends InitializationMethod

trait Smoothing {
  def forecast(data: List[Double]): Array[Double]
}
