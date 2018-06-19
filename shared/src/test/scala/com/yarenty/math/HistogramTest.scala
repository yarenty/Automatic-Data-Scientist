package com.yarenty.math

import com.yarenty.testhelpers.TimeSeriesData
import org.junit.Assert.{assertArrayEquals, assertEquals}
import org.junit.Test


class HistogramTest {




  @Test
  def testHistogram(): Unit = {
    val test = new Histogram(TimeSeriesData.data).default
    assertEquals(8, test.length)
    assertArrayEquals(Array(21, 45, 40, 35, 11, 7, 7, 2), test)
  }


}
