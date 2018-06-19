package com.yarenty.ml.preprocessing.normalization

import breeze.numerics.pow
import com.yarenty.ml.utils.h2o.{Helper, VecNormalizators}
import com.yarenty.testhelpers.AllH2OTests
import org.junit.{Assert, Before, Test}
import water.fvec.Vec

import scala.util.Random

class VecNormalizatorsTest extends AllH2OTests{

  var vec: Vec = _


  // just for testing speed ;-)
  def zeroOneNoWriter(v: Vec): Vec = { //0 to 1
    val out = Vec.makeZero(v.length)
    val max = v.max
    for (i <- 0 until v.length.toInt) {
      out.set(i, v.at(i) / max)
    }
    out
  }


  @Before
  def prepareVesc(): Unit = {
    vec = Vec.makeZero(1000) //100000
    for (i <- 0 until vec.length.toInt) {
      vec.set(i, Random.nextDouble * 100.0)
    }
  }

  // @Test // this is only to prove that writer should be used - usually 10 time quicker
  def testSpeed(): Unit = {
    VecNormalizators.ZeroToOne(vec)
    zeroOneNoWriter(vec)
    VecNormalizators.ZeroToOne(vec)
    zeroOneNoWriter(vec)
    VecNormalizators.ZeroToOne(vec)
    zeroOneNoWriter(vec)

    val t1 = System.nanoTime
    VecNormalizators.ZeroToOne(vec)
    val t2 = System.nanoTime
    zeroOneNoWriter(vec)
    val t3 = System.nanoTime

    val writer = (t2 - t1) / 10000
    val noWriter = (t3 - t2) / 10000

    println(s"Not writer: $noWriter - Writer: $writer") // scalastyle:ignore
    Assert.assertTrue(writer < noWriter)
  }

  @Test
  def testZeroOne(): Unit = {
    vec = Vec.makeZero(5)
    for (i <- 0 until vec.length.toInt) {
      vec.set(i, i.toDouble)
    }
    val out = VecNormalizators.ZeroToOne(vec)
    Assert.assertArrayEquals(Array(0.0, 0.25, 0.5, 0.75, 1.0), Helper.vecToArray(out), 0.0001)
  }


  @Test
  def testMaxMin(): Unit = {
    vec = Vec.makeZero(5)
    for (i <- 0 until vec.length.toInt) {
      vec.set(i, (i + 5).toDouble)
    }
    val out = VecNormalizators.MinMax(vec)
    Assert.assertArrayEquals(Array(0.0, 0.25, 0.5, 0.75, 1.0), Helper.vecToArray(out), 0.0001)
  }


  @Test
  def testPow(): Unit = {
    vec = Vec.makeZero(5)
    for (i <- 0 until vec.length.toInt) {
      vec.set(i, (i + 1).toDouble)
    }
    val out = VecNormalizators.Pow(vec, 2)
    Assert.assertArrayEquals(Array(1.0, 4.0, 9.0, 16.0, 25.0), Helper.vecToArray(out), 0.0001)
  }

  @Test
  def testLog(): Unit = {
    vec = Vec.makeZero(5)
    for (i <- 0 until vec.length.toInt) {
      vec.set(i, pow(10, i).toDouble)
    }
    val out = VecNormalizators.Log10(vec)
    Assert.assertArrayEquals(Array(1.0, 10.0, 100.0, 1000.0, 10000.0), Helper.vecToArray(vec), 0.0001)
    Assert.assertArrayEquals(Array(0.0, 1.0, 2.0, 3.0, 4.0), Helper.vecToArray(out), 0.0001)
  }


  @Test
  def testSqrt(): Unit = {
    vec = Vec.makeZero(5)
    for (i <- 0 until vec.length.toInt) {
      vec.set(i, pow(i, 2).toDouble)
    }
    val out = VecNormalizators.Sqrt(vec)
    Assert.assertArrayEquals(Array(0.0, 1.0, 4.0, 9.0, 16.0), Helper.vecToArray(vec), 0.0001)
    Assert.assertArrayEquals(Array(0.0, 1.0, 2.0, 3.0, 4.0), Helper.vecToArray(out), 0.0001)
  }
}
