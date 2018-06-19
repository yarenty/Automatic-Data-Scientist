package com.yarenty.testhelpers

import breeze.linalg._
import breeze.plot._

import scala.collection.mutable.ArrayBuffer


object Visualization {


  def doPics(yArr: Array[Double], pArr: Array[Double],
             yName: String,
             outliers: Array[Double] = null, oName: String = "outliers"
            ): Unit = {
    doPictures(Range.Double(0.0, yArr.length.toDouble, 1.0).toArray, yArr, pArr, yName, outliers = outliers, oName = oName)
  }


  def doPictures(xArr: Array[Double], yArr: Array[Double], pArr: Array[Double],
                 yName: String, pName: String = "prediction",
                 outliers: Array[Double] = null, oName: String = "outliers"

                ): Unit = {
    val x = new DenseVector(xArr)
    val y = new DenseVector(yArr)
    val pred = new DenseVector(pArr)

    val f = Figure(yName)
    val p = f.subplot(0)
    p += plot(x, y, name = yName)
    p += plot(x, pred, name = pName)

    if (outliers != null) {
      val (i, o) = toSparse(xArr, outliers)
      p += plot(new DenseVector(i), new DenseVector(o), shapes = true, colorcode = "red", name = oName)
    }

    p.legend = true


    f.saveas(s"src/test/resources/${yName}.png")
    print(yName + " PNG output\n") // scalastyle:ignore

  }


  def doPicturesWithAnomaly(yArr: Array[Double], pArr: Array[Double],
                            yName: String,
                            outliers: Array[Int]
                           ): Unit = {
    val pName: String = "prediction"
    val oName: String = "outliers"
    val xArr = Range.Double(0.0, yArr.length.toDouble, 1.0).toArray
    val x = new DenseVector(xArr)
    val y = new DenseVector(yArr)
    val pred = new DenseVector(pArr)

    val f = Figure(yName)
    f.width = 1200
    val p = f.subplot(0)
    p.legend = true
    p += plot(x, y, name = yName)
    p += plot(x, pred, name = pName)

    if (outliers != null) {
      val (i, o) = getValues(outliers, yArr)
      p += plot(new DenseVector(i), new DenseVector(o), shapes = true, colorcode = "red", name = oName)
    }
    f.saveas(s"src/test/resources/${yName}.png")
    print(yName + " PNG output\n") // scalastyle:ignore
  }


  def getValues(outliers: Array[Int], yArr: Array[Double]): (Array[Double], Array[Double]) = {
    val out = Array.ofDim[Double](outliers.length)
    val iout = Array.ofDim[Double](outliers.length)
    var idx = 0
    for (i <- outliers) {
      out(idx) = yArr(i)
      iout(idx) = i.toDouble
      idx += 1
    }
    (iout, out)
  }

  def toSparse(time: Array[Double], in: Array[Double]): (Array[Double], Array[Double]) = {
    val idx = new ArrayBuffer[Double]()
    val buf = new ArrayBuffer[Double]()

    for (i <- in.indices) {
      if (in(i) != Double.NaN && in(i) != 0.0d) {
        idx += time(i)
        buf += in(i)
      }
    }
    (idx.toArray, buf.toArray)
  }

}


