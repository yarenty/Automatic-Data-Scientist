package com.yarenty.ml.data.transformations.smoothing

import org.junit.Assert.assertEquals
import org.junit.Test

class DoubleExpSmoothingTest {

  val data = List(362.0, 385.0, 432.0, 341.0, 382.0, 409.0, 498.0, 387.0, 473.0, 513.0, 582.0, 474.0, 544.0, 582.0, 681.0, 557.0, 628.0, 707.0, 773.0, 592.0, 627.0, 725.0, 854.0, 661.0)

  @Test
  def forecastNISTDataAvgAll(): Unit = {
    val m = 4
    val alpha = 0.5
    val gamma = 0.6

    val prediction = new DoubleExpSmoothing(alpha,gamma, AvgAll, m).forecast(data).toList
    // These are the expected results
    val expected = List(362.0, 375.0, 396.0, 440.8, 387.76, 380.012, 398.33439999999996, 481.89527999999996, 439.707136, 471.6009232, 519.96753984, 597.260586208, 544.9289335295999, 553.4844271315199, 585.316845793024, 679.4380013858688, 627.7671787665306, 637.5016138269024, 702.7183472090176, 789.4112097373699, 683.0342780803352, 630.5355288277173, 681.6254955530932, 823.382830249853, 749.0466485232771, 755.9018819216276, 762.7571153199782, 769.6123487183288)
    assertEquals(expected, prediction)
  }

  @Test
  def forecastNISTDataAvg4(): Unit = {
    val m = 4
    val alpha = 0.5
    val gamma = 0.6

    val prediction = new DoubleExpSmoothing(alpha,gamma, Avg4, m).forecast(data).toList
    // These are the expected results
    val expected =List(362.0, 355.0, 372.0, 422.0, 377.2, 376.74, 399.688, 485.1556, 442.94272000000007, 473.853464, 521.0527968, 597.4366241600001, 544.5975505920001, 552.9987486304, 584.8997230604799, 679.1802933573761, 627.6664904986112, 637.5096419196454, 702.778325054269, 789.4791691052999, 683.0858403992255, 630.5634239264207, 681.633188512092, 823.3781142513002, 749.0371428455143, 755.8852285653784, 762.7333142852424, 769.5814000051065)
    assertEquals(expected, prediction)
  }

  @Test
  def forecastNISTDataDiff(): Unit = {
    val m = 4
    val alpha = 0.5
    val gamma = 0.6

    val prediction = new DoubleExpSmoothing(alpha,gamma, Diff, m).forecast(data).toList
    // These are the expected results
    val expected = List(362.0, 385.0, 408.0, 450.2, 393.04, 381.64799999999997, 397.65759999999995, 480.26512, 438.08934400000004, 470.4746528, 519.42491136, 597.1725672319999, 545.0946249983999, 553.72726638208, 585.525407159296, 679.5668554001153, 627.8175229004904, 637.4975997805309, 702.6883582863918, 789.3772300534047, 683.0084969208897, 630.5215812783653, 681.6216490735936, 823.3851882491298, 749.0514013621589, 755.9102085997529, 762.769015837347, 769.6278230749409)
    assertEquals(expected, prediction)
  }

  @Test(expected = classOf[AssertionError])
  def forecastNISTDataAvgTooSmall(): Unit = {
    val y = List(362.0, 385.0, 432.0)
    val m = 4
    val alpha = 0.5
    val gamma = 0.6
    val prediction = new DoubleExpSmoothing(alpha,gamma, AvgAll, m).forecast(y)
  }

}
