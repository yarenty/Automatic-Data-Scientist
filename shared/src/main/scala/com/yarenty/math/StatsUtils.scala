package com.yarenty.math

import org.apache.commons.math3.distribution.ChiSquaredDistribution
import org.apache.commons.math3.exception.{DimensionMismatchException, NotPositiveException}
import org.apache.commons.math3.linear.{LUDecomposition, RealMatrix, RealVector, SingularValueDecomposition}
import org.apache.commons.math3.util.{FastMath, MathArrays}

// scalastyle:off
object StatsUtils {
  def logLoss(actual: Double, predicted: Double, sigma: Double): Double = {
    val p = pdf(actual, predicted, sigma)
    if (p == 0.0d) return 0.0d
    -Math.log(p)
  }

  /**
    * @return value of probabilistic density function
    */
  def pdf(x: Double, x_hat: Double, sigma: Double): Double = {
    if (sigma == 0.0d) return 0.0d
    val diff = x - x_hat
    val numerator = Math.exp(-0.5d * diff * diff / sigma)
    val denominator = Math.sqrt(2.0d * Math.PI) * Math.sqrt(sigma)
    numerator / denominator
  }

  def logLoss(actual: RealVector, predicted: RealVector, sigma: RealMatrix): Double = {
    val p = pdf(actual, predicted, sigma)
    if (p == 0.0d) return 0.0d
    -Math.log(p)
  }

  /**
    * pdf(x, x_hat) = exp(-0.5 * (x-x_hat) * inv(Σ) * (x-x_hat)T) / ( 2π^0.5d * det(Σ)^0.5)
    *
    * @return value of probabilistic density function
    * @see https://en.wikipedia.org/wiki/Multivariate_normal_distribution#Density_function
    */
  def pdf(x: RealVector, x_hat: RealVector, sigma: RealMatrix): Double = {
    val dim = x.getDimension
    assert(x_hat.getDimension == dim, "|x| != |x_hat|, |x|=" + dim + ", |x_hat|=" + x_hat.getDimension)
    assert(sigma.getRowDimension == dim, "|x| != |sigma|, |x|=" + dim + ", |sigma|=" + sigma.getRowDimension)
    assert(sigma.isSquare, "Sigma is not square matrix")
    val LU = new LUDecomposition(sigma)
    val detSigma = LU.getDeterminant
    val denominator = Math.pow(2.0d * Math.PI, 0.5d * dim) * Math.pow(detSigma, 0.5d)
    if (denominator == 0.0d) { // avoid divide by zero
      return 0.0d
    }
    var invSigma: RealMatrix = null
    val solver = LU.getSolver
    if (!solver.isNonSingular) {
      val svd = new SingularValueDecomposition(sigma)
      invSigma = svd.getSolver.getInverse // least square solution

    }
    else invSigma = solver.getInverse
    //EigenDecomposition eigen = new EigenDecomposition(sigma);
    //double detSigma = eigen.getDeterminant();
    //RealMatrix invSigma = eigen.getSolver().getInverse();
    val diff = x.subtract(x_hat)
    val premultiplied = invSigma.preMultiply(diff)
    val sum = premultiplied.dotProduct(diff)
    val numerator = Math.exp(-0.5d * sum)
    numerator / denominator
  }

  /**
    * @param mu1    mean of the first normal distribution
    * @param sigma1 variance of the first normal distribution
    * @param mu2    mean of the second normal distribution
    * @param sigma2 variance of the second normal distribution
    * @return the Hellinger distance between two normal distributions
    * @see https://en.wikipedia.org/wiki/Hellinger_distance#Examples
    */
  def hellingerDistance(mu1: Double, sigma1: Double, mu2: Double, sigma2: Double): Double = {
    val sigmaSum = sigma1 + sigma2
    if (sigmaSum == 0.0d) return 0.0d
    val numerator = Math.pow(sigma1, 0.25d) * Math.pow(sigma2, 0.25d) * Math.exp(-0.25d * Math.pow(mu1 - mu2, 2d) / sigmaSum)
    val denominator = Math.sqrt(sigmaSum / 2d)
    if (denominator == 0.0d) return 1.0d
    1.0d - numerator / denominator
  }

  /**
    * @param mu1    mean vector of the first normal distribution
    * @param sigma1 covariance matrix of the first normal distribution
    * @param mu2    mean vector of the second normal distribution
    * @param sigma2 covariance matrix of the second normal distribution
    * @return the Hellinger distance between two multivariate normal distributions
    * @see https://en.wikipedia.org/wiki/Hellinger_distance#Examples
    */
  def hellingerDistance(mu1: RealVector, sigma1: RealMatrix, mu2: RealVector, sigma2: RealMatrix): Double = {
    val muSub = mu1.subtract(mu2)
    val sigmaMean = sigma1.add(sigma2).scalarMultiply(0.5d)
    val LUsigmaMean = new LUDecomposition(sigmaMean)
    val denominator = Math.sqrt(LUsigmaMean.getDeterminant)
    if (denominator == 0.0d) return 1.0d
    val sigmaMeanInv = LUsigmaMean.getSolver.getInverse
    // has inverse iff det != 0
    val sigma1Det = MatrixUtils.det(sigma1)
    val sigma2Det = MatrixUtils.det(sigma2)
    val numerator = Math.pow(sigma1Det, 0.25d) * Math.pow(sigma2Det, 0.25d) * Math.exp(-0.125d * sigmaMeanInv.preMultiply(muSub).dotProduct(muSub))
    1.0d - numerator / denominator
  }

  /**
    * @param observed means non-negative vector
    * @param expected means positive vector
    * @return p value
    */
  def chiSquareTest(observed: Array[Double], expected: Array[Double]): Double = {
    val distribution = new ChiSquaredDistribution(expected.length - 1.0d)
    1.0d - distribution.cumulativeProbability(chiSquare(observed, expected))
  }

  /**
    * @param observed means non-negative vector
    * @param expected means positive vector
    * @return chi2 value
    */
  def chiSquare(observed: Array[Double], expected: Array[Double]): Double = {
    if (observed.length < 2) throw new DimensionMismatchException(observed.length, 2)
    if (expected.length != observed.length) throw new DimensionMismatchException(observed.length, expected.length)
    MathArrays.checkPositive(expected)
    for (d <- observed) {
      if (d < 0.0d) throw new NotPositiveException(d)
    }
    var sumObserved = 0.0d
    var sumExpected = 0.0d
    for (i <- observed.indices) {
      sumObserved += observed(i)
      sumExpected += expected(i)

    }
    var ratio = 1.0d
    var rescale = false
    if (FastMath.abs(sumObserved - sumExpected) > 10e-6) {
      ratio = sumObserved / sumExpected
      rescale = true
    }
    var sumSq = 0.0d
    for (i <- observed.indices) {
      if (rescale) {
        val dev = observed(i) - ratio * expected(i)
        sumSq += dev * dev / (ratio * expected(i))
      }
      else {
        val dev = observed(i) - expected(i)
        sumSq += dev * dev / expected(i)
      }

    }
    sumSq
  }

  /**
    * This method offers effective calculation for multiple entries rather than calculation
    * individually
    *
    * @param observeds means non-negative matrix
    * @param expecteds means positive matrix
    * @return (chi2 value[], p value[])
    */
  def chiSquare(observeds: Array[Array[Double]], expecteds: Array[Array[Double]]): (Array[Double], Array[Double]) = {
    assert(observeds.length == expecteds.length)
    val len = expecteds.length
    val lenOfEach = expecteds(0).length
    val distribution = new ChiSquaredDistribution(lenOfEach - 1.0d)
    val chi2s = new Array[Double](len)
    val ps = new Array[Double](len)

    for (i <- 0 until len) {
      chi2s(i) = chiSquare(observeds(i), expecteds(i))
      ps(i) = 1.0d - distribution.cumulativeProbability(chi2s(i))
    }
    (chi2s, ps)
  }

  
  def mean(data: Array[Double]): Double = org.apache.commons.math3.stat.StatUtils.mean(data)
  def median(data: Array[Double]): Double = org.apache.commons.math3.stat.StatUtils.percentile(data, 50.0)
  
  def naCnt(data: Array[Double]): Double = {
    var n = 0 
    for (d <- data) if (d == null || d.isNaN) n = n+1
    n
  }
  
  def variance3(data: Array[Double]): Double = org.apache.commons.math3.stat.StatUtils.variance(data)

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

// scalastyle:on
