package com.yarenty.ml.data

import org.junit.{Assert, Test}

/**
  * Created by yarenty on 17/11/2016.
  */
// scalastyle:off
class CovarianceTest {


  val expected = Array(1.0, 2.0, 4, 8)



  @Test
  def testCovarianceDouble(): Unit = {
    val observedDouble = Array(2.0, 4, 8, 16.0)
    Assert.assertEquals(new Covariance(expected, observedDouble).covariance, 19.166666666666664, 1e-12)
  }

  @Test
  def testCovarianceTriple(): Unit = {
    val observedTriple = Array(3.0, 6, 12, 24.0)
    Assert.assertEquals(new Covariance(expected, observedTriple).covariance, 28.75, 1e-12)
  }

  @Test
  def testCovariancePlus1(): Unit = {
    val observedPlus1 = Array(2.0, 3.0, 5.0, 9.0)
    Assert.assertEquals(new Covariance(expected, observedPlus1).covariance, 9.583333333333332, 1e-12)
  }

  @Test
  def testCovariancePlus11(): Unit = {
    val observedPlus11 = Array(12.0, 13.0, 15.0, 19.0)
    Assert.assertEquals(new Covariance(expected, observedPlus11).covariance, 9.583333333333332, 1e-12)
  }

  @Test
  def testCovarianceSmallChange(): Unit = {
    val observedSmallChange = Array(2.0, 3, 6, 12.0) //!!!
    Assert.assertEquals(new Covariance(expected, observedSmallChange).covariance, 13.916666666666666, 1e-6)
  }

  @Test
  def testCovarianceDifferent(): Unit = {
    val observedDifferent = Array(67.0, 3.9, 43.0, 12.0)
    Assert.assertEquals(new Covariance(expected, observedDifferent).covariance, -43.108333333333334, 1e-6)
  }

  @Test
  def testCovarianceWithZero(): Unit = {
    val observedWithZero = Array(6.0, 3.9, 43.0, 0.0)
    Assert.assertEquals(new Covariance(expected, observedWithZero).covariance, -4.191666666666667, 1e-6)
  }

  @Test
  def testCovarianceWithNAN(): Unit = {

    val observedWithNAN = Array(67.0, 3.9, 43.0, Double.NaN)
    Assert.assertEquals(new Covariance(expected, observedWithNAN).covariance, Double.NaN, 1.0)
  }

  @Test
  def testCovarianceWithMax(): Unit = {
    val observedWithMax = Array(67.0, 3.9, 43.0, Double.MaxValue) //!!!!
    Assert.assertEquals(Double.PositiveInfinity, new Covariance(expected, observedWithMax).covariance, 1.0)
  }

  @Test
  def testCovarianceWithMin(): Unit = {
    val observedWithMin = Array(67.0, 3.9, 43.0, Double.MinValue) //!!!!
    Assert.assertEquals(Double.NegativeInfinity, new Covariance(expected, observedWithMin).covariance, 1.0)
  }

  @Test(expected = classOf[java.lang.AssertionError])
  def testCovarianceLess(): Unit = {
    val observedLess = Array(3.9, Double.MaxValue)
    //  an[org.apache.commons.math3.exception.MathIllegalArgumentException] should be thrownBy
    Assert.assertEquals(new Covariance(expected, observedLess).covariance, Double.NaN, 1.0)
  }

  @Test(expected = classOf[java.lang.AssertionError])
  def testCovarianceMore(): Unit = {

    val observedMore = Array(67.0, 3.9, 43.0, 73.0, 65.3, 545.0, Double.MaxValue)
    Assert.assertEquals(new Covariance(expected, observedMore).covariance, Double.NaN, 1.0)
  }

  @Test(expected = classOf[java.lang.AssertionError])
  def testCovarianceEmpty(): Unit = {

    val observedEmpty: Array[Double] = Array()
    Assert.assertEquals(new Covariance(expected, observedEmpty).covariance, Double.NaN, 1.0)
  }

}

// scalastyle:on