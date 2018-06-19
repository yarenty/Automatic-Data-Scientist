package com.yarenty.math.correlation

import org.junit.{Assert, Test}

/**
  * Created by yarenty on 17/11/2016.
  */
// scalastyle:off
class CoefficientCorrelationTest {


  val expected = Array(1.0, 2.0, 4, 8)
  val correlation = CoefficientCorrelation

  @Test
  def testCoeffiicientDouble(): Unit = {
    val observedDouble = Array(2.0, 4, 8, 16.0)
    Assert.assertEquals(correlation.correlate(expected, observedDouble), 1.0, 1e-12)
  }

  @Test
  def testCoeffiicientTriple(): Unit = {
    val observedTriple = Array(3.0, 6, 12.0, 24.0)
    Assert.assertEquals(correlation.correlate(expected, observedTriple), 1.0, 1e-12)
  }

  @Test
  def testCoeffiicientPlus1(): Unit = {
    val observedPlus1 = Array(2.0, 3.0, 5.0, 9.0)
    Assert.assertEquals(correlation.correlate(expected, observedPlus1), 1.0, 1e-12)
  }

  @Test
  def testCoeffiicientPlus11(): Unit = {
    val observedPlus11 = Array(12.0, 13.0, 15.0, 19.0)
    Assert.assertEquals(correlation.correlate(expected, observedPlus11), 1.0, 1e-12)
  }

  @Test
  def testCoeffiicientSmallChange(): Unit = {
    val observedSmallChange = Array(2.0, 3, 6, 12.0)
    Assert.assertEquals(correlation.correlate(expected, observedSmallChange), 0.9989975293745961, 1e-6) //  0.9989975  on Pearsons
  }

  @Test
  def testCoeffiicientDifferent(): Unit = {
    val observedDifferent = Array(67.0, 3.9, 43.0, 12.0)
    Assert.assertEquals(correlation.correlate(expected, observedDifferent), -0.47908963182533154, 1e-6)
  }

  @Test
  def testCoeffiicientWithZero(): Unit = {
    val observedWithZero = Array(6.0, 3.9, 43.0, 0.0)
    Assert.assertEquals(correlation.correlate(expected, observedWithZero), -0.06768439373944653, 1e-6)
  }

  @Test(expected = classOf[AssertionError])
  def testCoeffiicientWithNAN(): Unit = {
    val observedWithNAN = Array(67.0, 3.9, 43.0, Double.NaN, 1.0)
    Assert.assertEquals(correlation.correlate(expected, observedWithNAN), Double.NaN, 1.0)
  }

  @Test
  def testCoeffiicientWithMax(): Unit = {
    val observedWithMax = Array(67.0, 3.9, 43.0, Double.MaxValue)
    Assert.assertEquals(correlation.correlate(expected, observedWithMax), Double.NaN, 1.0)
  }

  @Test
  def testCoeffiicientWithMin(): Unit = {
    val observedWithMin = Array(67.0, 3.9, 43.0, Double.MinValue) //!!!!
    Assert.assertEquals(correlation.correlate(expected, observedWithMin), Double.NaN, 1.0)
  }

  @Test(expected = classOf[AssertionError])
  def testCoeffiicientLess(): Unit = {
    val observedLess = Array(3.9, Double.MaxValue)
    Assert.assertEquals(correlation.correlate(expected, observedLess), Double.NaN, 1.0)
  }

  @Test(expected = classOf[AssertionError])
  def testCoeffiicientMore(): Unit = {
    val observedMore = Array(67.0, 3.9, 43.0, 73.0, 65.3, 545.0, Double.MaxValue)
    Assert.assertEquals(correlation.correlate(expected, observedMore), 0.4461837747895351, 1e-6) // there is no check if there is more

  }

  @Test(expected = classOf[AssertionError])
  def testCoeffiicientEmpty(): Unit = {
    val observedEmpty: Array[Double] = Array()
    Assert.assertEquals(correlation.correlate(expected, observedEmpty), Double.NaN, 1.0)
  }
}

// scalastyle:on