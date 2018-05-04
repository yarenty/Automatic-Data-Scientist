package com.yarenty.ml.data.correlation

import org.junit.{Assert, Test}

/**
  * Created by yarenty on 17/11/2016.
  */
// scalastyle:off
class CosineCorrelationTest {
  val expected = Array(1.0, 2.0, 4, 8)

  val correlation = CosineSimilatiry

  @Test
  def testCosinusDouble {
    val observedDouble = Array(2.0, 4, 8, 16.0)
    Assert.assertEquals(correlation.correlate(expected, observedDouble), 1.0, 1e-12)
  }

  @Test
  def testCosinusTriple {
    val observedTriple = Array(3.0, 6, 12.0, 24.0)
    Assert.assertEquals(correlation.correlate(expected, observedTriple), 1.0, 1e-12)
  }

  @Test
  def testCosinusPlus1 {
    val observedPlus1 = Array(2.0, 3.0, 5.0, 9.0)
    Assert.assertEquals(correlation.correlate(expected, observedPlus1), 0.9942991232100196, 1e-12)
  }

  @Test
  def testCosinusPlus11 {
    val observedPlus11 = Array(12.0, 13.0, 15.0, 19.0)
    Assert.assertEquals(correlation.correlate(expected, observedPlus11), 0.90437948026465, 1e-12)
  }

  @Test
  def testCosinusSmallChange {
    val observedSmallChange = Array(2.0, 3, 6, 12.0)
    Assert.assertEquals(correlation.correlate(expected, observedSmallChange), 0.9993597462719618, 1e-6) //  0.9989975  on Pearsons
  }

  @Test
  def testCosinusDifferent {

    val observedDifferent = Array(67.0, 3.9, 43.0, 12.0)
    Assert.assertEquals(correlation.correlate(expected, observedDifferent), 0.4612834767202293, 1e-6)
  }

  @Test
  def testCosinusWithZero {
    val observedWithZero = Array(6.0, 3.9, 43.0, 0.0)
    Assert.assertEquals(correlation.correlate(expected, observedWithZero), 0.46231229893444836, 1e-6)
  }

  @Test
  def testCosinusWithNAN {
    val observedWithNAN = Array(67.0, 3.9, 43.0, Double.NaN)
    Assert.assertEquals(correlation.correlate(expected, observedWithNAN), Double.NaN, 1.0)
  }

  @Test
  def testCosinusWithMax {
    val observedWithMax = Array(67.0, 3.9, 43.0, Double.MaxValue) //!!!!
    Assert.assertEquals(correlation.correlate(expected, observedWithMax), Double.NaN, 1.0)
  }

  @Test
  def testCosinusWithMin {
    val observedWithMin = Array(67.0, 3.9, 43.0, Double.MinValue) //!!!!
    Assert.assertEquals(correlation.correlate(expected, observedWithMin), Double.NaN, 1.0)
  }

  @Test(expected = classOf[AssertionError])
  def testCosinusLess {
    val observedLess = Array(3.9, Double.MaxValue)
    Assert.assertEquals(correlation.correlate(expected, observedLess), Double.NaN, 1.0)
  }

  @Test(expected = classOf[AssertionError])
  def testCosinusMore {

    val observedMore = Array(67.0, 3.9, 43.0, 73.0, 65.3, 545.0, Double.MaxValue)
    Assert.assertEquals(correlation.correlate(expected, observedMore), 0.8337282561692554, 1e-6) // there is no check if there is more
  }

  @Test(expected = classOf[AssertionError])
  def testCosinusEmpty {
    val observedEmpty: Array[Double] = Array()
    Assert.assertEquals(correlation.correlate(expected, observedEmpty), Double.NaN, 1.0)
  }
}

//scalastyle:on
