package com.yarenty.ml.preprocessing

import com.yarenty.Log
import com.yarenty.math.StatsUtils
import org.joda.time.DateTime

import scala.collection.mutable.ArrayBuffer


/**
  * C(C)2018 by yarenty
  */
object NANProcessor {

  val NANStaticValue: Double = -1.0
  val NANTimeBuckets: Int = 24 * 4

  def apply(dt: Array[Array[Double]], NANAlgo: NANProcessingType): Unit = {

    for (i <- dt.indices) {
      this.apply(dt(i), NANAlgo)
    }

  }

  def apply(v: Array[Double], NANAlgo: NANProcessingType): Unit = {

    nonNAN(v)
    NANAlgo match {
      case None => nonNAN(v)
      case Zero =>
        setNAN(v, 0.0)
      case ConstValue =>
        setNAN(v, NANStaticValue)
      case Mean =>
        val mean = StatsUtils.mean(v)
        Log.info(s"NAN:: NAN set to MEAN: ${mean}")
        setNAN(v, mean)
      case Median =>
        val median = StatsUtils.median(v)
        Log.info(s"NAN:: NAN set to MEDIAN: ${median}")
        setNAN(v, median)
      case LastGoodValue =>
        val mean = StatsUtils.mean(v)
        lastGoodNAN(v, mean)
      case _ => nonNAN(v)

    }
  }

  def nonNAN(v: Array[Double]): Array[Double] = {
    for (i <- 0 until v.length.toInt) {
      if (v(i) == null) v(i) = Double.NaN
    }
    Log.info(s"NAN:: Length: ${v.length} NAN: ${StatsUtils.naCnt(v)} => ${(StatsUtils.naCnt(v) * 100 / v.length).toInt}% ")
    v
  }

  def setNAN(v: Array[Double], value: Double): Array[Double] = {
    for (i <- 0 until v.length.toInt) {
      if (v(i).isNaN) v(i) = value
    }
    v
  }

  //fill with last good known value - if beginning with NAN set to mean
  def lastGoodNAN(v: Array[Double], mean: Double): Array[Double] = {
    var lastGoodValue = mean
    for (i <- 0 until v.length.toInt) {
      if (v(i).isNaN) {
        v(i) = lastGoodValue
      } else {
        lastGoodValue = v(i)
      }
    }
    v
  }

  def apply(v: Array[Double], timeColumn: Array[Long], NANAlgo: NANProcessingType): Unit = {
    nonNAN(v)
    NANAlgo match {
      case DailyAverage =>
        if (StatsUtils.naCnt(v) < 0.5 * v.length) {
          dailyAverageNAN(v, timeColumn)
        } else {
          dailyAverageSoftenedNAN(v, timeColumn)
        }
      case DailyAverageSoftened =>
        dailyAverageSoftenedNAN(v, timeColumn)
    }
  }

  def dailyAverageNAN(v: Array[Double], timeVec: Array[Long]): Array[Double] = {
    Log.info(s"NAN:: Daily periods: ${NANTimeBuckets} ")
    // 1. get time buckets
    // 2. create ArrayOf  RingBuffer, averages
    val buckets = Array.ofDim[ArrayBuffer[Double]](NANTimeBuckets)
    val averages = Array.ofDim[Double](NANTimeBuckets)
    for (i <- 0 until NANTimeBuckets) buckets(i) = new ArrayBuffer[Double](1)

    // 3. for each non NAN - add value to ring buffer on specific ID
    for (i <- 0 until v.length.toInt) {
      if (!v(i).isNaN) {
        buckets(getIDX(timeVec(i))) += v(i)
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
      if (v(i).isNaN) {
        v(i) = averages(getIDX(timeVec(i)))
      }
    }

    v
  }

  def getIDX(t: Long): Int = {
    val dt = new DateTime(t)
    val den = NANTimeBuckets / 24
    dt.getHourOfDay * den + dt.getMinuteOfHour / (60 / den)
  }

  def dailyAverageSoftenedNAN(v: Array[Double], timeVec: Array[Long]): Array[Double] = {
    Log.info(s"NAN:: Softened / daily periods: ${NANTimeBuckets} ")

    // 1. get time buckets
    // 2. create ArrayOf  RingBuffer, averages
    val buckets = Array.ofDim[ArrayBuffer[Double]](NANTimeBuckets)
    val averages = Array.ofDim[Double](NANTimeBuckets)
    for (i <- 0 until NANTimeBuckets) buckets(i) = new ArrayBuffer[Double](1)

    // 3. for each non NAN - add value to ring buffer on specific ID
    for (i <- 0 until v.length.toInt) {
      if (!v(i).isNaN) {
        buckets(getIDX(timeVec(i))) += v(i)
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
      if (v(i).isNaN) {
        v(i) = averages(getIDX(timeVec(i)))
      }
    }
    v
  }

  sealed abstract class NANProcessingType

  case object None extends NANProcessingType

  case object Zero extends NANProcessingType

  case object ConstValue extends NANProcessingType

  case object Mean extends NANProcessingType

  case object Median extends NANProcessingType

  case object LastGoodValue extends NANProcessingType

  case object DailyAverage extends NANProcessingType

  case object DailyAverageSoftened extends NANProcessingType // for really bad data
}
