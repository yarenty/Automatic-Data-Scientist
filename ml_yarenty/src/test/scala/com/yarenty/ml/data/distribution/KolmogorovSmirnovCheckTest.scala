package com.yarenty.ml.data.distribution


import java.util.Random

import com.yarenty.testhelpers.TimeSeriesData
import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test

class KolmogorovSmirnovCheckTest {


  @Test
  def testKolmogorovSmirnov(): Unit = {
    val test = new KolmogorovSmirnovCheck().getDistribution(TimeSeriesData.data)

    println(test)
//    assertEquals(Levy(0.0, 0.5), test)
//    assertTrue(test._2>0.0)

  }


  @Test
  def testKolmogorovSmirnovScalled(): Unit = {
    val rnd = new Random()
    val d = TimeSeriesData.data.toList
    val d2 = d.map(x => (x - d.min) / (d.max - d.min))
    val test = new KolmogorovSmirnovCheck().getDistribution(d2.toArray)


    println(d2.mkString(", "))
    println(test)

//    assertTrue(test._2>0.0)
//    assertEquals(Exponential(0.37565522332964196), test)
  }

  @Test
  def testKolmogorovSmirnovRandomized(): Unit = {
    val rnd = new Random()
    val d = TimeSeriesData.data.toList
    val d2 = d.map(x => ((x - d.min) / (d.max - d.min) + rnd.nextDouble()) / 2.0) ++ List(0.0, 1.0) //to have proper borders
    val test = new KolmogorovSmirnovCheck().getDistribution(d2.toArray)


    println(d2.mkString(", "))
    println(test)

//    assertTrue(test._2 > 0.0002)
  }


  @Test
  def testKolmogorovSmirnovRandomized2(): Unit = {
    val rnd = new Random()
    val d2 = TimeSeriesData.data.toList.map(x => (x - 400) / 500 + rnd.nextDouble()) ++ List(0.0, 3.0)
    val test = new KolmogorovSmirnovCheck().getDistribution(d2.toArray)


    println(d2.mkString(", "))
    println(test)

//    assertTrue(test._2>0.0)
  }


  @Test
  def testKolmogorovSmirnovRandom(): Unit = {
    val rnd = new Random()
    val d2 = rnd.doubles(100).toArray ++ Array(0.0, 1.0)
    val test = new KolmogorovSmirnovCheck().getDistribution(d2)

    println(d2.mkString(", "))
    println(test)
//    assertTrue(test._2>0.0)
  }
}
