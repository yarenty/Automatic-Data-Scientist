package com.yarenty.ml.data.correlation

import com.yarenty.ml.data.Covariance
import com.yarenty.ml.data.distribution._
import org.junit.{Assert, Test}

/**
  * Created by yarenty on 17/11/2016.
  */
class CompareCorrelations {


  //scalastyle:off
  val expected = Array(1.0, 2.0, 4, 8)
  val observedDouble = Array(2.0, 4, 8, 16.0)
  val observedTriple = Array(3.0, 6, 12.0, 24.0)
  val observedPlus1 = Array(2.0, 3.0, 5.0, 9.0)
  val observedPlus11 = Array(12.0, 13.0, 15.0, 19.0)
  val observedSmallChange = Array(2.0, 3, 6, 12.0)
  val observedDifferent = Array(67.0, 3.9, 43.0, 12.0)
  val observedWithZero = Array(6.0, 3.9, 43.0, 0.0)
  val divided = expected.map(x => 1 / x)
  val dividedProportionaly = expected.map(x => (10 - x) / 10)
  val opposite=Array(8.0,4,2,1)
  //scalastyle:on

  var correlation: Correlation = CoefficientCorrelation


  @Test
  def checkCoefficient() {
    correlation = CoefficientCorrelation
    Assert.assertEquals(correlation.correlate(expected, observedDouble), 1.0, 1e-12)
    Assert.assertEquals(correlation.correlate(expected, observedTriple), 1.0, 1e-12)
    Assert.assertEquals(correlation.correlate(expected, observedPlus1), 1.0, 1e-12)
    Assert.assertEquals(correlation.correlate(expected, observedPlus11), 1.0, 1e-12)
    Assert.assertEquals(correlation.correlate(expected, observedSmallChange), 0.9989975293745961, 1e-6)
    Assert.assertEquals(correlation.correlate(expected, observedDifferent), -0.47908963182533154, 1e-6)
    Assert.assertEquals(correlation.correlate(expected, observedWithZero), -0.06768439373944653, 1e-6)
    Assert.assertEquals(correlation.correlate(expected, divided), -0.8434782608695652, 1e-6)
    Assert.assertEquals(correlation.correlate(expected, dividedProportionaly), -1.0, 1e-6)
  }

  @Test
  def checkCosinus() { //(plus is not ideal)
    correlation = CosineSimilatiry
    Assert.assertEquals(correlation.correlate(expected, observedDouble), 1.0, 1e-12)
    Assert.assertEquals(correlation.correlate(expected, observedTriple), 1.0, 1e-12)
    Assert.assertEquals(correlation.correlate(expected, observedPlus1), 0.9942991232100196, 1e-12)
    Assert.assertEquals(correlation.correlate(expected, observedPlus11), 0.90437948026465, 1e-12)
    Assert.assertEquals(correlation.correlate(expected, observedSmallChange), 0.9993597462719618, 1e-6)
    Assert.assertEquals(correlation.correlate(expected, observedDifferent), 0.4612834767202293, 1e-6)
    Assert.assertEquals(correlation.correlate(expected, observedWithZero), 0.46231229893444836, 1e-6)
    Assert.assertEquals(correlation.correlate(expected, divided), 0.3764705882352941, 1e-6)
    Assert.assertEquals(correlation.correlate(expected, dividedProportionaly), 0.518343944838785, 1e-6)
  }



  @Test
  def checkKendalls() { //(do not differentiate small changes)
    correlation = Kendalls
    Assert.assertEquals(correlation.correlate(expected, observedDouble), 1.0, 1e-12)
    Assert.assertEquals(correlation.correlate(expected, observedTriple), 1.0, 1e-12)
    Assert.assertEquals(correlation.correlate(expected, observedPlus1), 1.0, 1e-12)
    Assert.assertEquals(correlation.correlate(expected, observedPlus11), 1.0, 1e-12)
    Assert.assertEquals(correlation.correlate(expected, observedSmallChange), 1.0, 1e-6) //  0.9989975  on Pearsons
    Assert.assertEquals(correlation.correlate(expected, observedDifferent), -0.3333333333333333, 1e-6)
    Assert.assertEquals(correlation.correlate(expected, observedWithZero), -0.3333333333333333, 1e-6)
    Assert.assertEquals(correlation.correlate(expected, divided), -1.0, 1e-6)
    Assert.assertEquals(correlation.correlate(expected, dividedProportionaly), -1.0, 1e-6)

  }

  @Test
  def checkChiSquare() { //- not sure what is the output
    correlation = ChiSquareCorrelation
    Assert.assertEquals(1.0,correlation.correlate(expected, observedDouble),  1e-12)
    Assert.assertEquals(1.0, correlation.correlate(expected, observedTriple), 1e-12)
    Assert.assertEquals(1.0, correlation.correlate(expected, observedPlus1),  1e-12)
    Assert.assertEquals(1.0, correlation.correlate(expected, observedPlus11),  1e-12)
    Assert.assertEquals(1.0, correlation.correlate(expected, observedSmallChange), 1e-6) //  0.9989975  on Pearsons
    Assert.assertEquals(1.0, correlation.correlate(expected, observedDifferent),  1e-6)
    Assert.assertEquals(1.0, correlation.correlate(expected, observedWithZero), 1e-6)
    Assert.assertEquals(1.0, correlation.correlate(expected, divided), 1e-6)
    Assert.assertEquals(1.0, correlation.correlate(expected, dividedProportionaly),  1e-6)
    Assert.assertEquals(1.0, correlation.correlate(expected, opposite),  1e-6)
  }

  @Test
  def checkPearsons() { //- looks best
    correlation = Pearsons
    Assert.assertEquals(correlation.correlate(expected, observedDouble), 1.0, 1e-12)
    Assert.assertEquals(correlation.correlate(expected, observedPlus1), 1.0, 1e-12)
    Assert.assertEquals(correlation.correlate(expected, observedTriple), 1.0, 1e-12)
    Assert.assertEquals(correlation.correlate(expected, observedPlus11), 1.0, 1e-12)
    Assert.assertEquals(correlation.correlate(expected, observedSmallChange), 0.9989975, 1e-6)
    Assert.assertEquals(correlation.correlate(expected, observedDifferent), -0.47908963182533176, 1e-6)
    Assert.assertEquals(correlation.correlate(expected, observedWithZero), -0.06768439373944674, 1e-6)
    Assert.assertEquals(correlation.correlate(expected, divided), -0.8434782608695653, 1e-6)
    Assert.assertEquals(correlation.correlate(expected, dividedProportionaly), -1.0, 1e-6)
  }

  @Test
  def checkSpearmans() { //  -  its ranked Parsons - so depends on how ranking is working on data (ie: small changes)
    correlation = Spearmans
    Assert.assertEquals(correlation.correlate(expected, observedDouble), 1.0, 1e-12)
    Assert.assertEquals(correlation.correlate(expected, observedTriple), 1.0, 1e-12)
    Assert.assertEquals(correlation.correlate(expected, observedPlus1), 1.0, 1e-12)
    Assert.assertEquals(correlation.correlate(expected, observedPlus11), 1.0, 1e-12)
    Assert.assertEquals(correlation.correlate(expected, observedSmallChange), 1.0, 1e-6) //!!!
    Assert.assertEquals(correlation.correlate(expected, observedDifferent), -0.39999999999999997, 1e-6)
    Assert.assertEquals(correlation.correlate(expected, observedWithZero), -0.39999999999999997, 1e-6)
    Assert.assertEquals(correlation.correlate(expected, divided), -1.0, 1e-6)
    Assert.assertEquals(correlation.correlate(expected, dividedProportionaly), -1.0, 1e-6)
  }


}
