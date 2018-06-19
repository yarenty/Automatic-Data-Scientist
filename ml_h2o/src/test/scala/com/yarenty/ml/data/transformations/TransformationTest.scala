package com.yarenty.ml.data.transformations


import org.junit.Assert.assertArrayEquals
import org.junit.Test


class TransformationTest {

  val data = List(1.0, 2.0, 3.0, 8.0, 101.0, 488, 204, 728, 585, 542)

  @Test
  def testLnTransform(): Unit = {
    val test = log(data)
    val expected = Array(0.0, 0.69, 1.09, 2.07, 4.61, 6.19, 5.31, 6.59, 6.37, 6.29)
    assertArrayEquals(expected, test.toArray, 0.01)
  }

  @Test
  def testRootTransform(): Unit = {
    val test = root(data, 2)
    val expected = Array(1.0, 1.41, 1.73, 2.82, 10.04, 22.09, 14.28, 26.98, 24.18, 23.28)
    assertArrayEquals(expected, test.toArray, 0.01)
  }

  @Test
  def testSqrtTransform(): Unit = {
    val test = sqrt(data)
    val expected = Array(1.0, 1.41, 1.73, 2.82, 10.04, 22.09, 14.28, 26.98, 24.18, 23.28)
    assertArrayEquals(expected, test.toArray, 0.01)
  }

  @Test
  def testCbrtTransform(): Unit = {
    val test = cbrt(data)
    val expected = Array(1.0, 1.252, 1.44, 2.0, 4.65, 7.87, 5.88, 8.99, 8.36, 8.15)
    assertArrayEquals(expected, test.toArray, 0.01)

  }
}
