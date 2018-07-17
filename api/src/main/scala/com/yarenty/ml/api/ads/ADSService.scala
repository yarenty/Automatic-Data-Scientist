package com.yarenty.ml.api.ads

import java.net.URI

import com.yarenty.Constants
import com.yarenty.io.FileUtils
import com.yarenty.ml.algorithms.timeseries.anomalyDetection.MovingWindowAnomalyDetection
import com.yarenty.ml.preprocessing.{EmptyCSVParser, KPIProcessor, NameRegularization, TimeProcessor}
import hex.tree.gbm.GBMModel
import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import water.fvec.{H2OFrame, Vec}
import water.util.Log


object ADSService {
  
  val timeColumn: String = "ClusterTime" //"Start_Time"

  // fixme 
  var model:GBMModel = null
  
  var output:String = null
  
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
  
  


  def AnomalyDetection(data: H2OFrame) : String = {
    
    if (output == null) {
      val mwAD = new MovingWindowAnomalyDetection(data, "kpi", timeColumn)
      val out = mwAD.process()

      model = mwAD.model

      output =
        s"""
           | Anomalies for data:
           | $data
           | 
           | Model
           | ${model}
           | 
           | Found ${out._2.length} anomalies at positions: ${out._2.mkString(", ")}.
           | 
           | ${getOutputString(data,out._3)}
           |
          """.stripMargin
      
    }
    
    output
  }

  
  
  private def getOutputString(data: H2OFrame, anomalies: Array[Double]):String ={
    
    val out = StringBuilder.newBuilder
    out.append("TIME;\t KPI; \t Anomaly \n")


    for (i <- 0 until data.vec(timeColumn).length.toInt) {
     val a =  if (anomalies(i).isNaN) "_" else "ANOMALY"
      
      out.append(new DateTime(data.vec(timeColumn).at8(i)).toString("yyyy-MM-dd HH:mm"))
        .append("; \t")
        .append(data.vec("kpi").at(i))
        .append("; \t")
        .append(a)
        .append("\n")
      
    }
    
    out.toString
  }
  //  private def save(nodeData: H2OFrame)(implicit session: Session): Unit = {
  //    Helper.saveCSV(nodeData, session.directory + session.inputFile + "_processed")
  //  }

}