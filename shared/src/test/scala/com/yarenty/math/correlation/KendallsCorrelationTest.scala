package com.yarenty.math.correlation

import org.junit.{Assert, Test}

/**
  * Created by yarenty on 17/11/2016.
  */
// scalastyle:off
class KendallsCorrelationTest {

  val expected = Array(1.0, 2.0, 4, 8)

  val correlation = Kendalls

  @Test
  def testKendallsDouble(): Unit = {
    val observedDouble = Array(2.0, 4, 8, 16.0)
    Assert.assertEquals(correlation.correlate(expected, observedDouble), 1.0, 1e-12)
  }

  @Test
  def testKendallsTriple(): Unit = {
    val observedTriple = Array(3.0, 6, 12.0, 24.0)
    Assert.assertEquals(correlation.correlate(expected, observedTriple), 1.0, 1e-12)
  }

  @Test
  def testKendallsPlus1(): Unit = {
    val observedPlus1 = Array(2.0, 3.0, 5.0, 9.0)
    Assert.assertEquals(correlation.correlate(expected, observedPlus1), 1.0, 1e-12)
  }

  @Test
  def testKendallsPlus11(): Unit = {
    val observedPlus11 = Array(12.0, 13.0, 15.0, 19.0)
    Assert.assertEquals(correlation.correlate(expected, observedPlus11), 1.0, 1e-12)
  }

  @Test
  def testKendallsSmallChange(): Unit = {
    val observedSmallChange = Array(2.0, 3, 6, 12.0)
    Assert.assertEquals(correlation.correlate(expected, observedSmallChange), 1.0, 1e-6) //  0.9989975  on Pearsons
  }

  @Test
  def testKendallsDifferent(): Unit = {
    val observedDifferent = Array(67.0, 3.9, 43.0, 12.0)
    Assert.assertEquals(correlation.correlate(expected, observedDifferent), -0.3333333333333333, 1e-6)
  }

  @Test
  def testKendallsWithZero(): Unit = {
    val observedWithZero = Array(6.0, 3.9, 43.0, 0.0)
    Assert.assertEquals(correlation.correlate(expected, observedWithZero), -0.3333333333333333, 1e-6)
  }

  @Test
  def testKendallsWithNAN(): Unit = {
    val observedWithNAN = Array(67.0, 3.9, 43.0, Double.NaN)
    Assert.assertEquals(0.3333333333333, correlation.correlate(expected, observedWithNAN), 1e-6)
  }

  @Test
  def testKendallsWithMax(): Unit = {
    val observedWithMax = Array(67.0, 3.9, 43.0, Double.MaxValue) //!!!!
    Assert.assertEquals(0.3333333333333, correlation.correlate(expected, observedWithMax), 1e-6)
  }

  @Test
  def testKendallsWithMin(): Unit = {
    val observedWithMin = Array(67.0, 3.9, 43.0, Double.MinValue) //!!!!
    Assert.assertEquals(-0.66666666666666, correlation.correlate(expected, observedWithMin), 1e-6)
  }

  @Test(expected = classOf[AssertionError])
  def testKendallsLess(): Unit = {
    val observedLess = Array(3.9, Double.MaxValue)
    //  an  [org.apache.commons.math3.exception.DimensionMismatchException] should be thrownBy 
    Assert.assertEquals(correlation.correlate(expected, observedLess), Double.NaN, 1.0)
  }

  @Test(expected = classOf[AssertionError])
  def testKendallsMore(): Unit = {
    val observedMore = Array(67.0, 3.9, 43.0, 73.0, 65.3, 545.0, Double.MaxValue)
    Assert.assertEquals(correlation.correlate(expected, observedMore), Double.NaN, 1.0)
  }

  @Test(expected = classOf[AssertionError])
  def testKendallsEmpty(): Unit = {
    val observedEmpty: Array[Double] = Array()
    Assert.assertEquals(correlation.correlate(expected, observedEmpty), Double.NaN, 1.0)
  }
}

// scalastyle:on
