package com.yarenty.ml.data.correlation

import org.junit.{Assert, Test}

/**
  * Created by yarenty on 17/11/2016.
  */
class SpearmansCorrelationTest {
  //scalastyle:off
  val expected = Array(1.0, 2.0, 4, 8) 
  val correlation = Spearmans
  
  @Test
  def testSpearmansDouble() {
    val observedDouble = Array(2.0, 4, 8, 16.0)
    Assert.assertEquals(correlation.correlate(expected, observedDouble), 1.0, 1e-12)
  }

  @Test
  def testSpearmansTriple() {
    val observedTriple = Array(3.0, 6, 12.0, 24.0)
    Assert.assertEquals(correlation.correlate(expected, observedTriple), 1.0, 1e-12)
  }

  @Test
  def testSpearmansPlus1() {
    val observedPlus1 = Array(2.0, 3.0, 5.0, 9.0)
    Assert.assertEquals(correlation.correlate(expected, observedPlus1), 1.0, 1e-12)
  }

  @Test
  def testSpearmansPlus11() {
    val observedPlus11 = Array(12.0, 13.0, 15.0, 19.0)
    Assert.assertEquals(correlation.correlate(expected, observedPlus11), 1.0, 1e-12)
  }

  @Test
  def testSpearmansSmallChange() {
    val observedSmallChange = Array(2.0, 3, 6, 12.0)
    Assert.assertEquals(correlation.correlate(expected, observedSmallChange), 1.0, 1e-6) //!!!
  }

  @Test
  def testSpearmansDifferent() {
    val observedDifferent = Array(67.0, 3.9, 43.0, 12.0)
    Assert.assertEquals(correlation.correlate(expected, observedDifferent), -0.39999999999999997, 1e-6)
  }

  @Test
  def testSpearmansWithZero() {
    val observedWithZero = Array(6.0, 3.9, 43.0, 0.0)
    Assert.assertEquals(correlation.correlate(expected, observedWithZero), -0.39999999999999997, 1e-6)
  }

  @Test
  def testSpearmansWithNAN() {
    val observedWithNAN = Array(67.0, 3.9, 43.0, Double.NaN) // that how NAN is handling
    Assert.assertEquals(correlation.correlate(expected, observedWithNAN), Double.NaN, 1.0)

    //an  [org.apache.commons.math3.exception.NotANumberException] should be thrownBy correlation.correlate(expected, observedWithNAN)
  }

  @Test
  def testSpearmansWithMax() {
    val observedWithMax = Array(67.0, 3.9, 43.0, Double.MaxValue) //!!!!
    Assert.assertEquals(correlation.correlate(expected, observedWithMax), 0.4, 1e-6)
  }

  @Test
  def testSpearmansWithMin() {
    val observedWithMin = Array(67.0, 3.9, 43.0, Double.MinValue) //!!!!
    Assert.assertEquals(correlation.correlate(expected, observedWithMin), -0.8, 1e-6)
  }

  @Test(expected = classOf[AssertionError])
  def testSpearmansLess() {
    val observedLess = Array(3.9, Double.MaxValue)
    Assert.assertEquals(correlation.correlate(expected, observedLess), Double.NaN, 1.0)
    //an  [org.apache.commons.math3.exception.DimensionMismatchException] should be thrownBy correlation.correlate(expected, observedLess)
  }

  @Test(expected = classOf[AssertionError])
  def testSpearmansMore() {
    val observedMore = Array(67.0, 3.9, 43.0, 73.0, 65.3, 545.0, Double.MaxValue)
    Assert.assertEquals(correlation.correlate(expected, observedMore), Double.NaN, 1.0)
    //an  [org.apache.commons.math3.exception.DimensionMismatchException] should be thrownBy correlation.correlate(expected, observedMore)
  }

  @Test(expected = classOf[AssertionError])
  def testSpearmansEmpty() {
    val observedEmpty: Array[Double] = Array()
    Assert.assertEquals(correlation.correlate(expected, observedEmpty), Double.NaN, 1.0)
    //an  [org.apache.commons.math3.exception.DimensionMismatchException] should be thrownBy correlation.correlate(expected, observedMore)
  }
  //scalastyle:on
}
