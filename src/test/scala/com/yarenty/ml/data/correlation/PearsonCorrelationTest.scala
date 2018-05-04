package com.yarenty.ml.data.correlation

import org.junit.{Assert, Test}

/**
  * Created by yarenty on 17/11/2016.
  */
// scalastyle:off
class PearsonCorrelationTest {


  val expected = Array(1.0, 2.0, 4, 8)

  val correlation = Pearsons

  @Test
  def testPearsonMixed(): Unit = { //yes it is not working on "distributions" :-(
    val observedDouble = Array(8.0, 4, 1, 2.0)
    Assert.assertEquals(correlation.correlate(expected, observedDouble), -0.7043478260869565, 1e-6)
  }


  @Test
  def testPearsonDouble(): Unit = {
    val observedDouble = Array(2.0, 4, 8, 16.0)
    Assert.assertEquals(correlation.correlate(expected, observedDouble), 1.0, 1e-12)
  }

  @Test
  def testPearsonPlus1(): Unit = {
    val observedPlus1 = Array(2.0, 3.0, 5.0, 9.0)
    Assert.assertEquals(correlation.correlate(expected, observedPlus1), 1.0, 1e-12)
  }

  @Test
  def testPearsonTriple(): Unit = {
    val observedTriple = Array(3.0, 6, 12.0, 24.0)
    Assert.assertEquals(correlation.correlate(expected, observedTriple), 1.0, 1e-12)
  }

  @Test
  def testPearsonPlus11(): Unit = {
    val observedPlus11 = Array(12.0, 13.0, 15.0, 19.0)
    Assert.assertEquals(correlation.correlate(expected, observedPlus11), 1.0, 1e-12)
  }

  @Test
  def testPearsonSmallChange(): Unit = {
    val observedSmallChange = Array(2.0, 3, 6, 12.0)
    Assert.assertEquals(correlation.correlate(expected, observedSmallChange), 0.9989975, 1e-6)
  }

  @Test
  def testPearsonDifferent(): Unit = {
    val observedDifferent = Array(67.0, 3.9, 43.0, 12.0)
    Assert.assertEquals(correlation.correlate(expected, observedDifferent), -0.47908963182533176, 1e-6)
  }

  @Test
  def testPearsonWithZero(): Unit = {
    val observedWithZero = Array(6.0, 3.9, 43.0, 0.0)
    Assert.assertEquals(correlation.correlate(expected, observedWithZero), -0.06768439373944674, 1e-6)
  }

  @Test
  def testPearsonWithNAN(): Unit = {
    val observedWithNAN = Array(67.0, 3.9, 43.0, Double.NaN)
    Assert.assertEquals(correlation.correlate(expected, observedWithNAN),-0.1949248862907216, 1e-6)
  }

  @Test
  def testPearsonWithMax(): Unit = {
    val observedWithMax = Array(67.0, 3.9, 43.0, Double.MaxValue) //!!!!
    Assert.assertEquals(correlation.correlate(expected, observedWithMax), Double.NaN, 1.0)
  }

  @Test
  def testPearsonWithMin(): Unit = {
    val observedWithMin = Array(67.0, 3.9, 43.0, Double.MinValue) //!!!!
    Assert.assertEquals(correlation.correlate(expected, observedWithMin), Double.NaN, 1.0)
  }

  @Test(expected = classOf[java.lang.AssertionError])
  def testPearsonLess(): Unit = {
    val observedLess = Array(3.9, Double.MaxValue)
    Assert.assertEquals(correlation.correlate(expected, observedLess), Double.NaN, 1.0)
  }

  @Test(expected = classOf[java.lang.AssertionError])
  def testPearsonMore(): Unit = {
    val observedMore = Array(67.0, 3.9, 43.0, 73.0, 65.3, 545.0, Double.MaxValue)
    Assert.assertEquals(correlation.correlate(expected, observedMore), Double.NaN, 1.0)
  }

  @Test(expected = classOf[java.lang.AssertionError])
  def testPearsonEmpty(): Unit = {
    val observedEmpty: Array[Double] = Array()
    Assert.assertEquals(correlation.correlate(expected, observedEmpty), Double.NaN, 1.0)
  }

}

// scalastyle:on
