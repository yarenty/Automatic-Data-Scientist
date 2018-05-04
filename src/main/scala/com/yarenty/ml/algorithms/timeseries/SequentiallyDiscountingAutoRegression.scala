package com.yarenty.ml.algorithms.timeseries

import com.yarenty.ml.algorithms.utils.{MatrixUtils, StatsUtils}

/**
  * Sequentially discounting auto-regression model learning algorithm.
  *
  * @param _r - smaller r - larger influence of past examples!
  * @param k
  *
  * (C)2018 by yarenty
  */
class SequentiallyDiscountingAutoRegression(val _r: Double, val k: Int) {
  assert(0.0d < _r && _r < 1.0d, "Invalid forgetfullness parameter r: " + _r)
  assert(k >= 1, "Invalid smoothing parameter k: " + k)

  private var _C = new Array[Double](k + 1)
  private var _initialized = false
  private var _mu = .0
  private var _sigma = .0
  private var _muOld = .0
  private var _sigmaOld = .0

  /**
    * @param x series of input in LIFO order
    * @param k AR window size
    * @return x_hat predicted x
    */
  def update(x: Array[Double], k: Int): Double = {
    assert(null != x, "x cannot be empty (null)") // scalastyle:ignore
    assert(x.length >= 1, "x.length MUST be greather than 1: " + x.length)
    assert(k >= 0, "k MUST be greather than or equals to 0: " + k)
    assert(k < _C.length, "k MUST be less than |C| but k=" + k + ", |C|=" + _C.length)
    val x_t = x(0)
    if (!_initialized) {
      this._mu = x_t
      this._sigma = 0.0d
      this._initialized = true
      return 0.0d // scalastyle:ignore
    }
    assert(k >= 1, "k MUST be greater than 0: " + k)

    // old parameters are accessible to compute the Hellinger distance
    this._muOld = _mu
    this._sigmaOld = _sigma

    // update mean vector
    // \hat{mu} := (1-r) \hat{µ} + r x_t
    this._mu = (1.0d - _r) * _mu + _r * x_t

    // update covariance matrices
    // C_j := (1-r) C_j + r (x_t - \hat{µ}) (x_{t-j} - \hat{µ})^T
    val C = this._C

    for (j <- 0 to k) {
      C(j) = (1.0d - _r) * C(j) + _r * (x_t - _mu) * (x(j) - _mu)
    }

    // solve A in the following Yule-Walker equation
    // C_j = ∑_{i=1}^{k} A_i C_{j-i} where j = 1..k, C_{-i} = C'_i
    val A = new Array[Double](k + 1)
    //    java.util.Arrays.fill(A, 0.0d)
    MatrixUtils.aryule(C, A, k)

    // estimate x
    // \hat{x} = \hat{µ} + ∑_{i=1}^k A_i (x_{t-i} - \hat{µ})
    var x_hat = _mu

    for (i <- 1 to k) {
      x_hat += A(i) * (x(i) - _mu)
    }

    // update model covariance
    // ∑ := (1-r) ∑ + r (x - \hat{x}) (x - \hat{x})'
    val diffx = x_t - x_hat
    this._sigma = (1.0d - _r) * _sigma + _r * diffx * diffx
    x_hat
  }

  def logLoss(actual: Double, predicted: Double): Double = StatsUtils.logLoss(actual, predicted, _sigma)

  def hellingerDistance: Double = StatsUtils.hellingerDistance(_muOld, _sigmaOld, _mu, _sigma)
}
