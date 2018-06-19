package com.yarenty.testhelpers

import org.joda.time.format.DateTimeFormat
import org.joda.time.{DateTime, DateTimeZone}
import org.junit.Ignore


class TestHelper {} // scalastyle:ignore - need class to find resources

@Ignore("Support for tests, but no actual tests here")
object TestHelper {

  def generateFileName(name: String): String = getFileName("prostate.csv").replace("prostate.csv", name)

  def getFileName(fileName: String): String = {
    var is = getClass.getResource(fileName)
    if (null == is) is = getClass.getClassLoader.getResource(fileName)
    if (null == is) classOf[TestHelper].getResource("/../resources/" + fileName)
    if (null == is) classOf[TestHelper].getResource("/../../resources/test/" + fileName)
    is.getPath
  }


  


  def dummyTimestamp(d: Array[Double]): Array[String] = {
    val out = Array.ofDim[String](d.length)
    val dtz = DateTimeZone.forID("UTC")
    val formatter = DateTimeFormat.forPattern("dd/MM/yyyy").withZone(dtz)
    val dfi = new DateTime()
    for (i <- 0 until d.length) {
      out(i) = formatter.print(dfi.plusDays(i))
    }
    out
  }
  

}
