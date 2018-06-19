package com.yarenty.ml.data.transformations.h2oframe

import com.yarenty.ml.data.transformations.{Lambda, Ln}
import org.apache.commons.math3.analysis.UnivariateFunction
import org.apache.commons.math3.optim.nonlinear.scalar.GoalType
import org.apache.commons.math3.optim.univariate.{BrentOptimizer, SearchInterval, UnivariateObjectiveFunction}
import org.apache.commons.math3.optim.{MaxEval, OptimizationData}
import water.fvec.{Frame, Vec}
import water.util.{Log, MathUtils}

object BoxCox {
  /**
    * Find the optimal lambda for a given time series data set and conduct transformation
    *
    * @param  data a List<Double> of time series data
    * @return Time series List<Double> with optimal Box Cox lambda transformation
    */
  def transform(data: Frame): Frame = transform(data, lambdaSearch(data))


  /**
    * Calculate a Box Cox Transformation for a given lambda
    *
    * @param  data a Frame of time series data
    * @param lam   desired lambda for transformation
    * @return Time series Frame with desired Box Cox transformation
    */
  def transform(data: Frame, lam: Array[Double]): Frame = {
    for (c <- 0 until data.numCols) {
      val lambda = lam(c)
      val transformation = if (lambda == 0) Ln else Lambda(lambda)
      transformation.transform(data.vec(c))
    }
    data
  }


  /**
    * Find the optimal lambda for a given time series data set with default lower/upper bounds for lambda search
    *
    * @param  data a Frame of time series data
    * @return Time series Frame with optimal Box Cox lambda transformation
    */
  def lambdaSearch(data: Frame): Array[Double] = lambdaSearch(data, -1, 2)


  /**
    * Find the optimal lambda for a given time series data set given lower/upper bounds for lambda search
    *
    * @param  data a Frame of time series data
    * @param lower lower bound for lambda search
    * @param upper upper bound for lambda search
    * @return Time series List<Double> with optimal Box Cox lambda transformation
    */
  def lambdaSearch(data: Frame, lower: Double, upper: Double): Array[Double] = {

    val lambda = new Array[Double](data.numCols)
    for (c <- 0 until data.numCols) {
      val v = data.vec(c)
      val solver = new BrentOptimizer(1e-10, 1e-14)
      val optData = List[OptimizationData](new MaxEval(100),
        GoalType.MINIMIZE,
        new SearchInterval(lower, upper),
        new UnivariateObjectiveFunction(new UnivariateFunction() {
          override def value(x: Double): Double = lambdaCV(v, x)
        })
      )
      lambda(c) = solver.optimize(optData: _*).getPoint
    }

    Log.info("Lambda:" + lambda.mkString("; "))
    lambda
  }


  /**
    * Compute the coefficient of variation
    *
    * @param v   a Vec of time series data
    * @param lam lambda
    * @return Coefficient of Variation
    */
  private def lambdaCV(v: Vec, lam: Double) = {
    val stats = new MathUtils.BasicStats(1)
    for (i <- 0 until v.length.toInt - 1) {
      val mean = (v.at(i) + v.at(i + 1)) / 2
      var sd = Math.sqrt(Math.pow(v.at(i) - mean, 2) + Math.pow(v.at(i + 1) - mean, 2))
      sd /= Math.pow(mean, 1 - lam)
      stats.add(sd, 1, 0)
    }

    val len = if (v.length % 2 == 0) v.length / 2 else v.length / 2 + 1
    stats.sigma(0) / stats.mean(0)
  }
}
