package com.yarenty.ml.algorithms.timeseries

import com.yarenty.ml.algorithms.timeseries.AbnormalDetector.{distance, findKNearestClasses, findNearestClasses}
import org.junit.{Assert, Test}

class AbnormalDetectorTest {

  @Test
  def testNearestClasses {

    //scalastyle:off
    val testInstances = Array(Array(5.0, 4.8, 7.5, 10.0), Array(3.2, 2.1, 4.3, 2.8))
    val trainPoints = Array(Array(3.9, 4.1, 6.2, 7.3), Array(4.5, 6.1, 8.3, 3.8), Array(5.2, 4.6, 7.4, 9.8), Array(5.2, 4.6, 7.4, 9.8), Array(5.1, 7.1, 4.4, 6.9))
    //scalastyle:on

    val d = distance(testInstances.head, trainPoints.head)
    Assert.assertEquals(3.2680269276736382, d, 0.0000001)

    for (i <- trainPoints.indices) {
      val buff = trainPoints.toBuffer
      buff.remove(i)
      val n = findNearestClasses(Array(trainPoints(i)), buff.toList.toArray)
      val x = if (n.head >= i) n.head + 1 else n.head
      println(s"$i -> $x") // scalastyle:ignore
    }

    val n = findNearestClasses(testInstances, trainPoints)
    Assert.assertArrayEquals(Array(2, 0), n)

    val nk = findKNearestClasses(testInstances, trainPoints, 3)
    Assert.assertArrayEquals(Array(2, 4), nk) // scalastyle:ignore
  }
}
