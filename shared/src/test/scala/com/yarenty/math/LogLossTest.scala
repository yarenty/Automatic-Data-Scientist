package com.yarenty.math

import org.junit.{Assert, Test}

class LogLossTest {

  @Test
  def logTest: Unit = {

    val x = Array(0.0, 0.1, 0.5, 10.0, 100.0, 1000.0)
    Assert.assertEquals(Double.PositiveInfinity, -Math.log(x(0)), 1e-3)
    Assert.assertEquals(2.3025850929940455, -Math.log(x(1)), 1e-3)
    Assert.assertEquals(0.6931471805599453, -Math.log(x(2)), 1e-3)
    Assert.assertEquals(-2.302585092994046, -Math.log(x(3)), 1e-3)
    Assert.assertEquals(-4.605170185988092, -Math.log(x(4)), 1e-3)
    Assert.assertEquals(-6.907755278982137, -Math.log(x(5)), 1e-3)


    Assert.assertEquals( 2.5066282746310002, Math.sqrt(2.0d * Math.PI), 1e-3)
    
  }

  @Test
  def logLossTest: Unit = {
    val actual = 1.1767592992100477
    val predicted = 1.047308023360584971464
    val sigma = 0.8023360584971464

    val p = StatsUtils.pdf(actual, predicted, sigma)
    Assert.assertEquals(0.4407543039667225, p, 1e-5)

    val out = StatsUtils.logLoss(actual, predicted, sigma)
    Assert.assertEquals(0.8192676926543371, out, 1e-5)

  }

}
