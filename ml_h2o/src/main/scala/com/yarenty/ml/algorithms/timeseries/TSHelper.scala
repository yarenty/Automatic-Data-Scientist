package com.yarenty.ml.algorithms.timeseries

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import water.fvec.{H2OFrame, Vec}
import water.parser.BufferedString
import water.util.Log

object TSHelper {


  def getDate(index: Int, data: H2OFrame, timeColumn:String): String = {
    data.vec(timeColumn).get_type() match {
      case Vec.T_STR =>
        val sb = new BufferedString
        data.vec(timeColumn).atStr(sb, index).toString

      case Vec.T_TIME =>
        val date = data.vec(timeColumn).at8(index)
        val fmt = DateTimeFormat.forPattern("yyyy/MM/dd HH:mm:ss")
        fmt.print(new DateTime(date))

      case Vec.T_CAT =>
        data.vec(timeColumn).domain()(data.vec(timeColumn).at8(index).toInt)

      case _ =>
        Log.err("Could not process date column!")
        ""
    }
  }
}
