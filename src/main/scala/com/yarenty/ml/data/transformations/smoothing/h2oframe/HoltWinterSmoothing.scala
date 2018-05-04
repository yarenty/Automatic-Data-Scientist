package com.yarenty.ml.data.transformations.smoothing.h2oframe

import water.fvec.{Frame, Vec}
import water.util.Log

/**
  * @param alpha  - Exponential smoothing coefficients for level, trend,
  *               seasonal components.
  * @param beta   - Exponential smoothing coefficients for level, trend,
  *               seasonal components.
  * @param gamma  - Exponential smoothing coefficients for level, trend,
  *               seasonal components.
  * @param period - A complete season's data consists of L periods. And we need
  *               to estimate the trend factor from one period to the next. To
  *               accomplish this, it is advisable to use two complete seasons;
  *               that is, 2L periods.
  * @param m      - Extrapolated future data points.
  *          - 4 quarterly,
  *          - 7 weekly,
  *          - 12 monthly
  *
  */
class HoltWinterSmoothing(alpha: Double, beta: Double, gamma: Double, period: Int, m: Int) extends Smoothing {


  /**
    * This method is the entry point. It calculates the initial values and
    * returns the forecast for the future m periods.
    *
    * @param y - Time series data.
    *
    */
  def forecast(y: Frame): Array[Double] = {
    assert(y != null, "Value of y should be not null")
    assert(m > 0, "Value of m must be greater than 0.")
    assert(m <= period, "Value of m must be <= period.")
    assert((alpha >= 0.0) && (alpha <= 1.0), "Value of Alpha should satisfy 0.0 <= alpha <= 1.0")
    assert((beta >= 0.0) && (beta <= 1.0), "Value of Beta should satisfy 0.0 <= beta <= 1.0")
    assert((gamma >= 0.0) && (gamma <= 1.0), "Value of Gamma should satisfy 0.0 <= gamma <= 1.0")

    var forecast: Array[Double] = null
    for (v <- y.vecs) {
      val n = v.length.toInt
      val seasons = n / period
      val a0 = calculateInitialLevel(v)
      val b0 = calculateInitialTrend(v, period)
      val initialSeasonalIndices = calculateSeasonalIndices(v, period, seasons)

      Log.debug(s"Total observations: $n, Seasons $seasons, Periods $period")
      Log.debug("Initial level value a0: " + a0)
      Log.debug("Initial trend value b0: " + b0)

      forecast = calculateHoltWinters(v, a0, b0, initialSeasonalIndices)
    }
    forecast
  }


  /**
    * This method realizes the Holt-Winters equations.
    *
    * @param v
    * @param a0
    * @param b0
    * @param initialSeasonalIndices
    * @return - Forecast for m periods.
    */
  private def calculateHoltWinters(v: Vec,
                                   a0: Double, b0: Double,
                                   initialSeasonalIndices: List[Double]): Array[Double] = {
    val n = v.length.toInt
    val smoothing = Array.ofDim[Double](n)
    val trend = Array.ofDim[Double](n)
    val seasonal = Array.ofDim[Double](n)
    val forecast = Array.ofDim[Double](n + m)

    smoothing(1) = a0
    trend(1) = b0

    for (i <- 0 until period) seasonal(i) = initialSeasonalIndices(i)

    // Start calculations
    for (i <- 2 until n) { // Calculate overall smoothing
      if ((i - period) >= 0) {
        smoothing(i) = alpha * v.at(i) / seasonal(i - period) + (1.0 - alpha) * (smoothing(i - 1) + trend(i - 1))
      } else {
        smoothing(i) = alpha * v.at(i) + (1.0 - alpha) * (smoothing(i - 1) + trend(i - 1))
      }

      // Calculate trend smoothing
      trend(i) = gamma * (smoothing(i) - smoothing(i - 1)) + (1 - gamma) * trend(i - 1)

      // Calculate seasonal smoothing
      if ((i - period) >= 0) seasonal(i) = beta * v.at(i) / smoothing(i) + (1.0 - beta) * seasonal(i - period)

      // Calculate forecast
      if ((i + m) >= period) {
        forecast(i + m) = (smoothing(i) + m * trend(i)) * seasonal(i - period + m)
      }

      Log.debug(s"i = $i, y = ${Math.round(v.at(i))}, S =  ${smoothing(i)}, Bt = ${trend(i)}, It = ${seasonal(i)}, F = ${forecast(i)}")

    }
    forecast
  }

  /**
    * See: http://robjhyndman.com/researchtips/hw-initialization/ 1st period's
    * average can be taken. But y[0] works better.
    *
    * @return - Initial Level value i.e. St[1]
    */
  private def calculateInitialLevel(v: Vec) = v.at(0)

  /**
    * See: http://www.itl.nist.gov/div898/handbook/pmc/section4/pmc435.htm
    *
    * @return - Initial trend - Bt[1]
    */
  private def calculateInitialTrend(v: Vec, period: Int) = {
    var sum = 0.0
    for (i <- 0 until period) {
      sum += (v.at(period + i) - v.at(i))
    }
    sum / (period * period)
  }

  /**
    * See: http://www.itl.nist.gov/div898/handbook/pmc/section4/pmc435.htm
    *
    * @return - Seasonal Indices.
    */
  private def calculateSeasonalIndices(v: Vec, period: Int, seasons: Int): List[Double] = {
    val n = v.length.toInt
    val seasonalAverage = Array.ofDim[Double](seasons)
    val seasonalIndices = Array.ofDim[Double](period)
    val averagedObservations = Array.ofDim[Double](n)

    for (i <- 0 until seasons) {
      for (j <- 0 until period) seasonalAverage(i) += v.at((i * period) + j)
      seasonalAverage(i) /= period
    }


    for (i <- 0 until seasons) {
      for (j <- 0 until period) averagedObservations((i * period) + j) = v.at((i * period) + j) / seasonalAverage(i)
    }

    for (i <- 0 until period) {
      for (j <- 0 until seasons) seasonalIndices(i) += averagedObservations((j * period) + i)
      seasonalIndices(i) /= seasons
    }

    seasonalIndices.toList
  }
}
