package com.yarenty.ml.data.transformations

import com.yarenty.ml.algorithms.utils.Stats
import org.apache.commons.math3.analysis.UnivariateFunction
import org.apache.commons.math3.optim.nonlinear.scalar.GoalType
import org.apache.commons.math3.optim.univariate.{BrentOptimizer, SearchInterval, UnivariateObjectiveFunction}
import org.apache.commons.math3.optim.{MaxEval, OptimizationData}

import scala.collection.mutable.ListBuffer

object BoxCox {
  /**
    * Find the optimal lambda for a given time series data set and conduct transformation
    *
    * @param  data a List<Double> of time series data
    * @return Time series List<Double> with optimal Box Cox lambda transformation
    */
  def transform(data: List[Double]): List[Double] = transform(data, lambdaSearch(data))

  /**
    * Calculate a Box Cox Transformation for a given lambda
    *
    * @param  data a List<Double> of time series data
    * @param lam   desired lambda for transformation
    * @return Time series List<Double> with desired Box Cox transformation
    */
  def transform(data: List[Double], lam: Double): List[Double] = if (lam == 0) {
    data.map(d => Math.log(d))
  } else {
    data.map(d => (Math.pow(d, lam) - 1.0) / lam)
  }


  /**
    * Find the optimal lambda for a given time series data set with default lower/upper bounds for lambda search
    *
    * @param  data a List<Double> of time series data
    * @return Time series List<Double> with optimal Box Cox lambda transformation
    */
  def lambdaSearch(data: List[Double]): Double = lambdaSearch(data, -1, 2)


  /**
    * Find the optimal lambda for a given time series data set given lower/upper bounds for lambda search
    *
    * @param  data a List<Double> of time series data
    * @param lower lower bound for lambda search
    * @param upper upper bound for lambda search
    * @return Time series List<Double> with optimal Box Cox lambda transformation
    */
  def lambdaSearch(data: List[Double], lower: Double, upper: Double): Double = {
    val solver = new BrentOptimizer(1e-10, 1e-14)
    val optData = List[OptimizationData](new MaxEval(100),
      GoalType.MINIMIZE,
      new SearchInterval(lower, upper),
      new UnivariateObjectiveFunction(new UnivariateFunction() {
        override def value(x: Double): Double = lambdaCV(data, x)
      })
    )

    val lambda = solver.optimize(optData: _*)
    lambda.getPoint
  }


  /**
    * Compute the coefficient of variation
    *
    * @param data a List<Double> of time series data
    * @param lam  lambda
    * @return Coefficient of Variation
    */
  private def lambdaCV(data: List[Double], lam: Double) = {
    val iter = data.iterator
    val avg = new ListBuffer[Double]()
    val result = new ListBuffer[Double]()
    while (iter.hasNext) {
      val l = new ListBuffer[Double]()
      l += iter.next
      if (iter.hasNext) l += iter.next
      avg += Stats.average(l.toList)
      result += Stats.standardDeviation(l.toList)
    }

    val out = result.toArray
    val av = avg.toArray
    for (i <- 0 until result.size.toInt) {
      out(i) = out(i) / Math.pow(av(i), 1 - lam)
    }

    Stats.standardDeviation(out.toList) / Stats.average(out.toList)
  }


}
