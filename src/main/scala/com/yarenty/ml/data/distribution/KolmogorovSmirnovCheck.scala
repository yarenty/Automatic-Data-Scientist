package com.yarenty.ml.data.distribution

import org.apache.commons.math3.stat.StatUtils
import org.apache.commons.math3.stat.inference.KolmogorovSmirnovTest

import scala.collection.mutable

/**
  * @see <a href="http://en.wikipedia.org/wiki/Kolmogorov-Smirnov_test">Kolmogorov-Smirnov Test</a>
  *
  *      This test is trying to find best distribution from :
  * - Beta, @see <a href="http://en.wikipedia.org/wiki/Beta_distribution">Beta distribution</a>
  * - Cauchy,  @see <a href="http://en.wikipedia.org/wiki/Cauchy_distribution">Cauchy distribution (Wikipedia)</a>
  * - ChiSquared, @see <a href="http://en.wikipedia.org/wiki/Chi-squared_distribution">Chi-squared distribution (Wikipedia)</a>
  * - ConstantReal,
  * - Exponential, @see <a href="http://en.wikipedia.org/wiki/Exponential_distribution">Exponential distribution (Wikipedia)</a>
  * - F, @see <a href="http://en.wikipedia.org/wiki/F-distribution">F-distribution (Wikipedia)</a>
  * - Gamma, @see <a href="http://en.wikipedia.org/wiki/Gamma_distribution">Gamma distribution (Wikipedia)</a>
  * - Gumbel, @see <a href="http://en.wikipedia.org/wiki/Gumbel_distribution">Gumbel Distribution (Wikipedia)</a>
  * - Laplace,  @see <a href="http://en.wikipedia.org/wiki/Laplace_distribution">Laplace distribution (Wikipedia)</a>
  * - Levy, @see <a href="http://en.wikipedia.org/wiki/L%C3%A9vy_distribution"> Levy distribution </a>
  * - Logistic, @see <a href="http://en.wikipedia.org/wiki/Logistic_distribution">Logistic Distribution (Wikipedia)</a>
  * - LogNormal,  @see <a href="http://en.wikipedia.org/wiki/Log-normal_distribution">Log-normal distribution (Wikipedia)</a>
  * - Nakagami,  @see <a href="http://en.wikipedia.org/wiki/Nakagami_distribution">Nakagami Distribution (Wikipedia)</a>
  * - Normal, @see <a href="http://en.wikipedia.org/wiki/Normal_distribution">Normal distribution (Wikipedia)</a>
  * - Pareto,  @see <a href="http://en.wikipedia.org/wiki/Pareto_distribution"> Pareto distribution (Wikipedia)</a>
  * - T, @see "<a href='http://en.wikipedia.org/wiki/Student&apos;s_t-distribution'>Student's t-distribution (Wikipedia)</a>"
  * - Triangular, @see <a href="http://en.wikipedia.org/wiki/Triangular_distribution">Triangular distribution (Wikipedia)</a>
  * - UniformReal, @see <a href="http://en.wikipedia.org/wiki/Uniform_distribution_(continuous)">Uniform distribution (continuous), at Wikipedia</a>
  * - Weibull @see <a href="http://en.wikipedia.org/wiki/Weibull_distribution">Weibull distribution (Wikipedia)</a>
  *
  *
  *      (C)2018 by yarenty
  *
  */

class KolmogorovSmirnovCheck {


  def getDistribution(data: Array[Double]): (Distribution, Double) = {

    //TODO - calculate proper input parameters!!

    val mean = StatUtils.mean(data)
    val min = StatUtils.min(data)
    val max = StatUtils.max(data)


    val distributions = List(new Beta, new Cauchy, new ChiSquared,
      new ConstantReal(mean), new Exponential(mean),
      new F, new Gamma, new Gumbel,
      new Laplace, new Levy,
      new Logistic, new LogNormal,
      new Nakagami, new Normal, new Pareto,
      new T, new Triangular,
      new UniformReal(min, max), new Weibull)


    val out = mutable.HashMap[Distribution, Double]()
    val test = new KolmogorovSmirnovTest()
    for (dist <- distributions) {
      out += dist -> test.kolmogorovSmirnovTest(dist.getDistribution(), data)
    }

    println(out.mkString(";\n"))

    val dist = out.maxBy(_._2)
    dist
  }

}
