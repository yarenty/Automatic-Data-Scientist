package com.yarenty.ml.algorithms.timeseries

import com.yarenty.ml.algorithms.LinearRegression
import org.junit.{Assert, Test}

class inearRegressionTest {
  /*
   * Test in R: x <- c(1,2,3,4); y <- c(1.0, 1.5, 4.15, 100.1); m <- lm(y ~ x)
   * resid(m): 19.305 -10.190 -37.535  28.420
   */
  @Test
  def testLinearRegr: Unit = {
    val a = Array(1.0, 1.5, 4.15, 100.1)
    val lsq = LinearRegression(a)
    val k = lsq.coeff_k
    val b = lsq.coeff_b
    Assert.assertEquals(k, 29.995, 0.001)
    Assert.assertEquals(b, -48.3, 0.0001)
    val r = lsq.residuals
    //    val r2 = ApplyLinReg(a, k, b)
    Assert.assertArrayEquals(Array(19.305, -10.189999999999998, -37.53499999999999, 28.42), r, 0.001)
  }

}
