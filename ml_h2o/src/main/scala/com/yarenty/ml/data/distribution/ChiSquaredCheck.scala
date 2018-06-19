package com.yarenty.ml.data.distribution

import com.yarenty.math.correlation.ChiSquareCorrelation
import org.apache.commons.math3.stat.StatUtils
import water.util.Log

import scala.collection.mutable

class ChiSquaredCheck {


  def getDistribution(data: Array[Double]): Distribution = {

    //TODO - calculate proper input parameters!!

    val mean = StatUtils.mean(data)
    val min = StatUtils.min(data)
    val max = StatUtils.max(data)


    val distributions = List(new Beta, new Cauchy, new ChiSquared,
      //new ConstantReal(mean), -- not working with chi :-)
      new Exponential(mean), new F,
      new Gamma, new Gumbel,
      new Laplace, new Levy,
      new Logistic, new LogNormal,
      new Nakagami, new Normal,
      new Pareto, new T, new Triangular,
      new UniformReal(min, max), new Weibull)


    val out = mutable.HashMap[Distribution, Double]()
    val test = ChiSquareCorrelation
    for (dist <- distributions) {
      Log.debug("checkin: " + dist)
      println(dist)
      out += dist -> test.correlate(dist.getSampleData(data.length), data, true)
    }

    Log.debug(out.mkString(";\n"))

    out.maxBy(_._2)._1
  }

}
