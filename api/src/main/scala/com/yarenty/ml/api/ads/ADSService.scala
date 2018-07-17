package com.yarenty.ml.api.ads

import java.net.URI

import com.yarenty.Constants
import com.yarenty.io.FileUtils
import com.yarenty.ml.algorithms.timeseries.anomalyDetection.MovingWindowAnomalyDetection
import com.yarenty.ml.preprocessing.{EmptyCSVParser, KPIProcessor, NameRegularization, TimeProcessor}
import water.fvec.{H2OFrame, Vec}
import water.util.Log


object ADSService {
  
  val timeColumn: String = "ClusterTime" //"Start_Time"

  
  def calculateTimeKPIS(data: H2OFrame): H2OFrame = {
    val (time: Vec, day: Vec, hour: Vec, minute: Vec) = TimeProcessor(
      if (data.vec(timeColumn).get_type == Vec.T_CAT) {
        data.vec(timeColumn).toStringVec
      } else {
        data.vec(timeColumn)
      }
    )

    //remove old
    data.remove(timeColumn)
    //nodeData.rename(session.timeColumn, session.timeColumn_old)

    data.add(Constants.DAY_OF_WEEK, day.toCategoricalVec)
    data.add(Constants.HOUR_OF_DAY, hour.toCategoricalVec)
    data.add(Constants.MINUTE_OF_DAY, minute.toCategoricalVec)
    //add new
    data.add(timeColumn, time)
    data
  }

  def calculateKPI(data:H2OFrame):H2OFrame = {
    val kpi = KPIProcessor(data.vec("L_E_RAB_SuccEst"), data.vec("L_E_RAB_AttEst"))
    data.add("kpi", kpi)
    data
  }
  
  


  def AnomalyDetection(data: H2OFrame) : (Boolean, List[Int], Array[Int], Array[Double]) = {
    
    val mwAD = new MovingWindowAnomalyDetection(data, "kpi", timeColumn)
    mwAD.process()
  }

  //  private def save(nodeData: H2OFrame)(implicit session: Session): Unit = {
  //    Helper.saveCSV(nodeData, session.directory + session.inputFile + "_processed")
  //  }

}