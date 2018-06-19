package com.yarenty.ml.data

package object transformations {

  def log(l: List[Double]): List[Double] = l.map(x => Math.log(x))

  def log10(l: List[Double]): List[Double] = l.map(Math.log10)

  def sqrt(l: List[Double]): List[Double] = l.map(Math.sqrt)

  def cbrt(l: List[Double]): List[Double] = l.map(Math.cbrt)

  def root(l: List[Double], root: Double): List[Double] = l.map(Math.pow(_, 1.0 / root))

}
