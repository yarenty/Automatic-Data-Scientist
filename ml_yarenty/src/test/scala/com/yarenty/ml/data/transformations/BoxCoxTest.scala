package com.yarenty.ml.data.transformations

import com.yarenty.testhelpers.{TestHelper, TimeSeriesData}
import org.junit.Assert.{assertArrayEquals, assertEquals}
import org.junit.Test

class BoxCoxTest {

  val data = List(501.0, 488, 504, 578, 545, 632, 728, 725, 585, 542)

  @Test
  def testBoxCox(): Unit = {
    val test = BoxCox.transform(data, 1.4)
    val exp = Array(4301.03, 4145.57, 4337.13, 5254.26, 4839.08, 5954.20, 7257.93, 7216.09, 5343.57, 4801.82)
    assertArrayEquals(test.toArray, exp, 0.01)
  }

  @Test
  def testBoxCoxAuto(): Unit = {
    val test = BoxCox.transform(data)
    val exp = Array(0.998, 0.997, 0.998, 0.998, 0.998, 0.998, 0.998, 0.998, 0.99, 0.99)
    assertArrayEquals(test.toArray, exp, 0.01)
  }

  @Test def testLambdaSearch(): Unit = {
    val ls = BoxCox.lambdaSearch(data)
    assertEquals(-0.9999242, ls, 0.01)
  }
}

//
//object BoxCoxTest {
//
//  //just to create charts for documentation
//  def main(args: Array[String]): Unit = {
//
//    val timestamp = TestHelper.dummyTimestamp(TimeSeriesData.data)
//
//    TwoPeriodicCharts.plot(TestHelper.generateFileName("transformations/BoxCoxAuto"), timestamp,
//      TimeSeriesData.data.toArray, "input data",
//      BoxCox.transform(TimeSeriesData.data.toList).toArray, "BoxCox with auto Lambda search")
//
//    TwoPeriodicCharts.plot(TestHelper.generateFileName("transformations/BoxCox"), timestamp,
//      TimeSeriesData.data.toArray, "input data",
//      BoxCox.transform(TimeSeriesData.data.toList, 1.4).toArray, "BoxCox (lambda=1.4)")
//
//  }
//}