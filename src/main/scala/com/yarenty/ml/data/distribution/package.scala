package com.yarenty.ml.data

import org.apache.commons.math3
import org.apache.commons.math3.distribution._

package object distribution {

  trait Distribution

  trait RealDistribution extends Distribution {
    def getDistribution(): org.apache.commons.math3.distribution.RealDistribution

    def getSampleData(length: Int): Array[Double] = {
      getDistribution().sample(length)
    }

  }

  trait IntegerDistribution extends Distribution

  //  trait Enumerated extends Distribution

  case class Beta(alpha: Double = 2, beta: Double = 5) extends RealDistribution {
    override def getDistribution(): math3.distribution.RealDistribution = new BetaDistribution(alpha, beta)
  }

  case class Cauchy(median: Double = 0, scale: Double = 1) extends RealDistribution {
    override def getDistribution(): math3.distribution.RealDistribution = new CauchyDistribution(median, scale)
  }

  case class ChiSquared(degreesOfFreedom: Double = 1) extends RealDistribution {
    override def getDistribution(): math3.distribution.RealDistribution = new ChiSquaredDistribution(degreesOfFreedom)
  }

  case class ConstantReal(v: Double) extends RealDistribution {
    override def getDistribution(): math3.distribution.RealDistribution = new ConstantRealDistribution(v)
  }

  case class Exponential(mean: Double) extends RealDistribution {
    override def getDistribution(): math3.distribution.RealDistribution = new ExponentialDistribution(mean)
  }

  case class F(numeratorDegreesOfFreedom: Double = 2, denominatorDegreesOfFreedom: Double = 1) extends RealDistribution {
    override def getDistribution(): math3.distribution.RealDistribution = new FDistribution(numeratorDegreesOfFreedom, denominatorDegreesOfFreedom)
  }

  case class Gamma(shape: Double = 1, scale: Double = 2) extends RealDistribution {
    override def getDistribution(): math3.distribution.RealDistribution = new GammaDistribution(shape, scale)
  }

  case class Gumbel(mu: Double = 2, beta: Double = 1) extends RealDistribution {
    override def getDistribution(): math3.distribution.RealDistribution = new GumbelDistribution(mu, beta)
  }

  case class Laplace(mu: Double = 0, beta: Double = 1) extends RealDistribution {
    override def getDistribution(): math3.distribution.RealDistribution = new LaplaceDistribution(mu, beta)
  }

  case class Levy(mu: Double = 0, c: Double = 1) extends RealDistribution {
    override def getDistribution(): math3.distribution.RealDistribution = new LevyDistribution(mu, c)
  }

  case class Logistic(mu: Double = 5, s: Double = 2) extends RealDistribution {
    override def getDistribution(): math3.distribution.RealDistribution = new LogisticDistribution(mu, s)
  }

  case class LogNormal(scale: Double = 0, shape: Double = 1) extends RealDistribution {
    override def getDistribution(): math3.distribution.RealDistribution = new LogNormalDistribution(scale: Double, shape: Double)
  }

  case class Nakagami(mu: Double = 5, omega: Double = 1) extends RealDistribution {
    override def getDistribution(): math3.distribution.RealDistribution = new NakagamiDistribution(mu: Double, omega: Double)
  }

  case class Normal(mean: Double = 0, sd: Double = 1) extends RealDistribution { //gaussian
    override def getDistribution(): math3.distribution.RealDistribution = new NormalDistribution(mean: Double, sd: Double)
  }

  case class Pareto(scale: Double = 1, shape: Double = 1) extends RealDistribution {
    override def getDistribution(): math3.distribution.RealDistribution = new ParetoDistribution(scale: Double, shape: Double)
  }

  case class T(degreesOfFreedom: Double = 2) extends RealDistribution {
    override def getDistribution(): math3.distribution.RealDistribution = new TDistribution(degreesOfFreedom: Double)
  }

  case class Triangular(a: Double = 0, c: Double = 3, b: Double = 4) extends RealDistribution {
    override def getDistribution(): math3.distribution.RealDistribution = new TriangularDistribution(a: Double, c: Double, b: Double)
  }

  case class UniformReal(lower: Double, upper: Double) extends RealDistribution {
    override def getDistribution(): math3.distribution.RealDistribution = new UniformRealDistribution(lower: Double, upper: Double)
  }

  case class Weibull(alpha: Double = 1, beta: Double = 5) extends RealDistribution {
    override def getDistribution(): math3.distribution.RealDistribution = new WeibullDistribution(alpha: Double, beta: Double)
  }

  //  case object EnumeratedReal extends RealDistribution with Enumerated
  //  case object EnumeratedInteger extends IntegerDistribution with Enumerated

  case object Binomial extends IntegerDistribution

  case object Hypergeometric extends IntegerDistribution

  case object Pascal extends IntegerDistribution

  case object Poisson extends IntegerDistribution

  case object UniformInteger extends IntegerDistribution

  case object Zipf extends IntegerDistribution


  //McDonald-Kreitman
}
