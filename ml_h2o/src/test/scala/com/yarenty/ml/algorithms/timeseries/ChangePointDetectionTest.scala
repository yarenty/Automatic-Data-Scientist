package com.yarenty.ml.algorithms.timeseries

import com.yarenty.testhelpers.{TestHelper, TimeSeriesData}
import org.junit.{Assert, Test}

class ChangePointDetectionTest {

  @Test
  def singleExponentialSmoothingTst: Unit = {
    val out = ChangePointDetection.detect(TimeSeriesData.no_anomaly)
    Assert.assertTrue(out._1)
    Assert.assertEquals(21, out._2.length)
    Assert.assertEquals(25, out._2(0))
    Assert.assertEquals(51, out._2(1))
  }

}
