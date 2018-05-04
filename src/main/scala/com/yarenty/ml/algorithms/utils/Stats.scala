package com.yarenty.ml.algorithms.utils


object Stats {


  def standardDeviation(data: List[Double]): Double = Math.sqrt(variance(data))

  def variance(data: List[Double]): Double = {
    val avg = average(data)
    data.map(d => (d - avg) * (d - avg)).sum / (data.length - 1)
  }

  def average(data: List[Double]): Double = {
    data.sum / data.length
  }

  def getMinimumIndex(data: List[Double]): Int = data.indexOf(data.min)

  def getMaximumIndex(data: List[Double]): Int = data.indexOf(data.max)


  def getAutoCovariance(data: List[Double], k: Int): Double = {
    if (k == 0) {
      variance(data)
    } else {
      val avg = average(data)

      val arr = data.toArray
      var total = 0.0
      for (i <- k until data.length) {
        total += (arr(i - k) - avg) * (arr(i) - avg)

      }
      total / data.length
    }
  }

  def getAutoCorrelation(data: List[Double], k: Int): Double = {
    getAutoCovariance(data, k) / variance(data)
  }

  def getAcf(data: List[Double], n: Int): Array[Double] = {
    val acfValues = new Array[Double](n + 1)
    for (i <- 0 to n) {
      acfValues(i) = getAutoCorrelation(data, i)
    }
    acfValues
  }


  def getPacf(data: List[Double], n: Int): Array[Double] = {
    val pacfValues = Array[Double](n + 1)
    val phi = Array.ofDim[Double](n + 1, n + 1)
    pacfValues(0) = 1.0
    phi(0)(0) = 1.0
    pacfValues(1) = getAutoCorrelation(data, 1)
    phi(1)(1) = pacfValues(1)


    for (i <- 2 to n) {
      for (j <- 1 until i - 1) phi(i - 1)(j) = phi(i - 2)(j) - phi(i - 1)(i - 1) * phi(i - 2)(i - 1 - j)

      var a = 0.0
      var b = 0.0
      for (j <- 1 until i) {
        a += phi(i - 1)(j) * getAutoCorrelation(data, i - j)
        b += phi(i - 1)(j) * getAutoCorrelation(data, j)
      }
      pacfValues(i) = (getAutoCorrelation(data, i) - a) / (1 - b)
      phi(i)(i) = pacfValues(i)

    }
    pacfValues
  }
}