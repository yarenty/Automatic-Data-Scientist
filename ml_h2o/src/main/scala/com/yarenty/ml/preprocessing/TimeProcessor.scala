package com.yarenty.ml.preprocessing

import org.joda.time.{DateTime, DateTimeZone}
import org.joda.time.format.DateTimeFormat
import water.fvec.Vec
import water.parser.BufferedString
import water.util.Log

/**
  * Created by yarenty on 05/12/2016.
  */
object TimeProcessor {

  def timeZone: String = "UTC"
  def timeFormat: String = "yyyyMMdd.HHmm"
  def fixedDateFormatSize: Boolean = true
  
  
  /**
    * Returns and regularize time column adding DayOfWeek, HourOfDay, MinuteOfDay)
    * @param date
    * @return (time, DayOfWeek, HourOfDay, MinuteOfDay)
    */
  // scalastyle:off
  def apply(date: Vec): (Vec, Vec, Vec, Vec) = {

    Log.info("Time processing ..... ")
    val dtTime = Vec.makeZero(date.length, Vec.T_TIME)
    val day = Vec.makeZero(date.length)
    val hour = Vec.makeZero(date.length)
    val minute = Vec.makeZero(date.length)

    val dtTimeW = dtTime.open()
    val dayW = day.open()
    val hourW = hour.open()
    val minuteW = minute.open()

    date.get_type() match {
      case Vec.T_TIME =>
        for (i <- 0L until date.length) {
          val dt = new DateTime(date.at8(i))
          dtTimeW.set(i, dt.getMillis)
          dayW.set(i, dt.getDayOfWeek)
          hourW.set(i, dt.getHourOfDay)
          minuteW.set(i, dt.getMinuteOfDay)
        }

      case Vec.T_STR =>

        val dtz = DateTimeZone.forID(timeZone)
        val formatter = DateTimeFormat.forPattern(timeFormat).withZone(dtz)

        val str = new BufferedString
        for (i <- 0L until date.length) {
          date.atStr(str, i)
          //TODO: fixme properly!!
          val dStr = str.toString
          val dt =
            if (fixedDateFormatSize) formatter.parseDateTime(dStr.substring(0, timeFormat.length ))
            else if (dStr.contains('+')) formatter.parseDateTime(dStr.split('+')(0))
            //            else if (dStr.contains('-')) formatter.parseDateTime(dStr.split('-')(0))
            // else if (dStr.endsWith("DST")) formatter.parseDateTime(dStr.dropRight(4))
            else
              formatter.parseDateTime(dStr)

          dtTimeW.set(i, dt.getMillis)
          dayW.set(i, dt.getDayOfWeek)
          hourW.set(i, dt.getHourOfDay)
          minuteW.set(i, dt.getMinuteOfDay)
        }

      case Vec.T_CAT =>
        val formatter = DateTimeFormat.forPattern(timeFormat)

        for (i <- 0L until date.length) {
          val str = date.domain()(date.at8(i).toInt)
          val dt = formatter.parseDateTime(str)

          dtTimeW.set(i, dt.getMillis)
          dayW.set(i, dt.getDayOfWeek)
          hourW.set(i, dt.getHourOfDay)
          minuteW.set(i, dt.getMinuteOfDay)
        }

      case Vec.T_NUM =>
        for (i <- 0L until date.length) {
          val dt = new DateTime(date.at8(i)*1000L) //unix type
          dtTimeW.set(i, dt.getMillis)
          dayW.set(i, dt.getDayOfWeek)
          hourW.set(i, dt.getHourOfDay)
          minuteW.set(i, dt.getMinuteOfDay)
        }
    }

    dtTimeW.close()
    dayW.close()
    hourW.close()
    minuteW.close()
    (dtTime, day, hour, minute)
  }
  // scalastyle:on
}

