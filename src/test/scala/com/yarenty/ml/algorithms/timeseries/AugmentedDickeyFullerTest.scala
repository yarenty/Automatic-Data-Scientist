package com.yarenty.ml.algorithms.timeseries

import java.util.Random

import org.junit.{Assert, Test}

/**
  * Created by yarenty on 10/12/2016.
  */
class AugmentedDickeyFullerTest {


  @Test
  def testLinearTrend() {
    val rand = new Random()
    val x = new Array[Double](100) // scalastyle:ignore
    for (i <- x.indices) {
      x(i) = (i + 1) + 5 * rand.nextDouble // scalastyle:ignore
    }
    val adf = new AugmentedDickeyFuller(x)
    Assert.assertTrue(adf.isNeedsDiff)
  }

  @Test
  def testLinearTrendWithOutlier() {
    val rand = new Random()
    val x = new Array[Double](100) // scalastyle:ignore
    for (i <- x.indices) {
      x(i) = (i + 1) + 5 * rand.nextDouble // scalastyle:ignore
    }
    x(50) = 100 // scalastyle:ignore
    val adf = new AugmentedDickeyFuller(x)
    Assert.assertTrue(adf.isNeedsDiff)
  }


}