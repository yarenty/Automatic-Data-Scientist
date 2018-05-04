package com.yarenty.ml.preprocessing

import com.yarenty.ml.utils.h2o.Helper
import org.apache.commons.math3.stat.descriptive.rank.Median
import org.joda.time.DateTime
import water.fvec.{H2OFrame, Vec}
import water.util.Log

import scala.collection.mutable.ArrayBuffer


/**
  * C(C)2018 by yarenty
  */
object NANProcessor {

  val NANStrings: Array[Array[String]] = Array(Array("NAN"))
  val NANStaticValue: Double = -1.0
  def timeColumn: String = "Time"
  val NANTimeBuckets: Int = 24 * 4


  sealed abstract class NANProcessingType

  case object None extends NANProcessingType

  case object Zero extends NANProcessingType

  case object ConstValue extends NANProcessingType

  case object Mean extends NANProcessingType

  case object Median extends NANProcessingType

  case object LastGoodValue extends NANProcessingType

  case object DailyAverage extends NANProcessingType

  case object DailyAverageSoftened extends NANProcessingType // for really bad data

  def apply(dt: H2OFrame, NANAlgo:NANProcessingType): Unit = {


    for (i <- dt.vecs.indices) {
      val v = dt.vec(i)
      if (v.isNumeric && i < NANStrings.length && null != NANStrings(i)) { //scalastyle:ignore
        Log.info(s"NAN:: processing col[$i]: ${dt.name(i)}; ")
        NANAlgo match {
          case None => nonNAN(v, NANStrings(i))
          case Zero =>
            nonNAN(v, NANStrings(i))
            setNAN(v, 0.0)
          case ConstValue =>
            nonNAN(v, NANStrings(i))
            setNAN(v, NANStaticValue)
          case Mean =>
            val n = nonNAN(v, NANStrings(i))
            Log.info(s"NAN:: NAN set to MEAN: ${n.mean}")
            setNAN(n, n.mean)
          case Median =>
            val n = nonNAN(v, NANStrings(i))
            val m = new Median()
            m.setData(Helper.vecToArrayNoNAN(n))
            val median = m.evaluate()
            Log.info(s"NAN:: NAN set to MEDIAN: ${median}")
            setNAN(n, median)
          case LastGoodValue =>
            val n = nonNAN(v, NANStrings(i))
            lastGoodNAN(n, n.mean)
          case DailyAverage =>
            val n = nonNAN(v, NANStrings(i))
            if (n.naCnt < 0.5 * n.length) {
              dailyAverageNAN(n, dt.vec(timeColumn))
            } else {
              dailyAverageSoftenedNAN(n, dt.vec(timeColumn))
            }
          case DailyAverageSoftened =>
            val n = nonNAN(v, NANStrings(i))
            dailyAverageSoftenedNAN(n, dt.vec(timeColumn))
        }
      }
    }
  }


  def nonNAN(v: Vec, nans: Array[String]): Vec = {
    Log.info(s"NAN:: replacing: ${nans.mkString(",")}")
    for (i <- 0 until v.length.toInt) {
      if (nans.contains(v.at(i).toString)) v.setNA(i)
    }
    Log.info(s"NAN:: Length: ${v.length} NAN: ${v.naCnt} => ${(v.naCnt * 100 / v.length).toInt}% ")
    v
  }

  def setNAN(v: Vec, value: Double): Vec = {
    for (i <- 0 until v.length.toInt) {
      if (v.at(i).isNaN) v.set(i, value)
    }
    v
  }

  def getIDX(t: Long): Int = {
    val dt = new DateTime(t)
    val den = NANTimeBuckets / 24
    dt.getHourOfDay * den + dt.getMinuteOfHour / (60 / den)
  }


  def dailyAverageNAN(v: Vec, timeVec: Vec): Vec = {
    Log.info(s"NAN:: Daily periods: ${NANTimeBuckets} ")

    // 1. get time buckets
    // 2. create ArrayOf  RingBuffer, averages
    val buckets = Array.ofDim[ArrayBuffer[Double]](NANTimeBuckets)
    val averages = Array.ofDim[Double](NANTimeBuckets)
    for (i <- 0 until NANTimeBuckets) buckets(i) = new ArrayBuffer[Double](1)

    // 3. for each non NAN - add value to ring buffer on specific ID
    for (i <- 0 until v.length.toInt) {
      if (!v.at(i).isNaN) {
        buckets(getIDX(timeVec.at8(i))) += v.at(i)
      }
    }

    // 4. process average - sum/size - if all buckets are not empty!
    for (i <- 0 until NANTimeBuckets) {
      if (buckets(i).isEmpty) {
        Log.err("NAN:: Not enough data to process daily average NAN.")
        return v //not enough data to create buckets!
      }
      averages(i) = buckets(i).sum / buckets(i).length
    }

    // 5. for each NAN - set average based on id
    for (i <- 0 until v.length.toInt) {
      if (v.at(i).isNaN) {
        v.set(i, averages(getIDX(timeVec.at8(i))))
      }
    }

    v
  }


  def dailyAverageSoftenedNAN(v: Vec, timeVec: Vec): Vec = {
    Log.info(s"NAN:: Softened / daily periods: ${NANTimeBuckets} ")

    // 1. get time buckets
    // 2. create ArrayOf  RingBuffer, averages
    val buckets = Array.ofDim[ArrayBuffer[Double]](NANTimeBuckets)
    val averages = Array.ofDim[Double](NANTimeBuckets)
    for (i <- 0 until NANTimeBuckets) buckets(i) = new ArrayBuffer[Double](1)

    // 3. for each non NAN - add value to ring buffer on specific ID
    for (i <- 0 until v.length.toInt) {
      if (!v.at(i).isNaN) {
        buckets(getIDX(timeVec.at8(i))) += v.at(i)
      }
    }

    // 4. process average - sum/size - if all buckets are not empty!
    for (i <- 0 until NANTimeBuckets) {
      if (buckets(i).isEmpty) {
        if (i == 0) averages(0) = 0.0 else averages(i) = averages(i - 1)
      } else {
        averages(i) = buckets(i).sum / buckets(i).length
      }
    }

    //5. smoothing
    for (i <- 0 until NANTimeBuckets) {
      if (i == 0) averages(0) = (averages(0) + averages(NANTimeBuckets - 1) + averages(1)) / 3
      else if (i == NANTimeBuckets - 1) averages(i) = (averages(i) + averages(i - 1) + averages(0)) / 3
      else averages(i) = (averages(i) + averages(i - 1) + averages(i + 1)) / 3
    }

    // 6. for each NAN - set average based on id
    for (i <- 0 until v.length.toInt) {
      if (v.at(i).isNaN) {
        v.set(i, averages(getIDX(timeVec.at8(i))))
      }
    }
    v
  }

  //fill with last good known value - if beginning with NAN set to mean
  def lastGoodNAN(v: Vec, mean: Double): Vec = {
    var lastGoodValue = mean
    for (i <- 0 until v.length.toInt) {
      if (v.at(i).isNaN) {
        v.set(i, lastGoodValue)
      } else {
        lastGoodValue = v.at(i)
      }
    }
    v
  }
}
