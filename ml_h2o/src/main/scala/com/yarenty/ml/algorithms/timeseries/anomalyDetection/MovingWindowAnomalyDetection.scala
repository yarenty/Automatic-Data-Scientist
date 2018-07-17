package com.yarenty.ml.algorithms.timeseries.anomalyDetection


import com.yarenty.Constants
import com.yarenty.ml.algorithms.timeseries.TSHelper
import com.yarenty.ml.utils.h2o.Helper
import hex.tree.gbm.GBMModel.GBMParameters
import hex.tree.gbm.{GBM, GBMModel}
import water.Key
import water.fvec.{Frame, H2OFrame, Vec}
import water.util.Log


class MovingWindowAnomalyDetection(data: H2OFrame, kpi: String, timeColumn: String) {

  /** DYNAMIC: anomaly detection - window style */
  val dynamicSplitPoint = 0.6
  val kpiVec: Vec = data.vec(kpi)

  def adName: String = "MovingWindow"
  
  var model:GBMModel = null

  def process(): (Boolean, List[Int], Array[Double], Array[Double]) = {
    val t = data.vec(timeColumn)
    val len = t.length.toInt - 1
    val period = (t.at(len) - t.at(0)) / (1000 * 60 * 60 * 24)

    Log.info(s"Moving window: processing $period days")


    val inputData: Key[Frame] = Key.make(s"${kpi}_AD_input").asInstanceOf[Key[Frame]]
      val inputFrame = new Frame(inputData)
      inputFrame.add(data.subframe(Array(timeColumn, Constants.DAY_OF_WEEK, Constants.HOUR_OF_DAY, Constants.MINUTE_OF_DAY, kpi)))
    val toSplit = H2OFrame(inputFrame)
    val (toTrain, toPredict) = split(toSplit, dynamicSplitPoint)

    val diff = detectGBM(H2OFrame(toTrain), H2OFrame(toPredict))

    val anomalies = findAnomalies(diff, toTrain.vec(kpi), toSplit.vec(kpi))
    val isAnomaly = anomalies.length > 0

    val aDates: List[String] = (for (a <- anomalies) yield TSHelper.getDate(a, data, timeColumn)) toList

//      obj.kpiStats(kpi).isAnomaly = isAnomaly
//      obj.kpiStats(kpi).anomalyIndexes = anomalies.toList
//      obj.kpiStats(kpi).anomalies = getAnomalies(kpiVec, anomalies)

    
    
    Log.info(s"AD:${kpi} (change:${kpiVec.sigma()}, thresh:${kpiVec.sigma()}) :: ${aDates.mkString(",")}")


    ( isAnomaly, anomalies.toList, Helper.vecToArray(getAnomalies(kpiVec, anomalies).vec("anomalies")),  Helper.vecToArray(diff))
  }


  private def split(in: H2OFrame, ratio: Double): (Frame, Frame) = {
    val keys = Array[String](s"$${kpi}_train", s"$${kpi}_real")
    val ratios = Array[Double](ratio)

    val frs = Helper.split(in, ratios)
    (frs(0), frs(1))
  }

  def findAnomalies(diff: Vec, t: Vec, full: Vec): Array[Int] = {
    val min = diff.maxs().min
    //assumption:  values close to zero should be compared with max  - ie: drop rate / others with min
    val zeros =
      if (t.max < 1.0 && t.mean < 0.5) true
      else if (t.max > 1.0 && t.mean / t.max < 0.5) true
      else false
    //normalize

    //    val n = t.mean - (t.mean - t.min) / 2
    //    val x = t.mean + (t.max - t.mean) / 2
    Log.info(s"ANOMALY!!! zero value is set to $zeros  where min|mean|max=${t.min}|${t.mean}|${t.max}")
    // must be outside threshold tunnel  and has biggest difference to predictions (1 of 5) ??
    Helper.vecToArray(full).zipWithIndex.filter(v =>
      ((zeros && v._1 > t.max) || (!zeros && v._1 < t.min)) && diff.at(v._2) > min
      //      (((zeros && v._1 > x) || (!zeros && v._1 < n)) && diff.at(v._2) > min)
    ).map(_._2)
    // all
    //    Helper.vecToArray(diff).zipWithIndex.filter(_._1 > min).map(_._2)
  }


  def detectGBM(train: H2OFrame, toPredict: H2OFrame): Vec = {
    val params = new GBMParameters()

    params._train = train.key
    params._response_column = kpi
    params._ignore_const_cols = true

    val key: Key[GBMModel] = Key.make(s"${kpi}_AD").asInstanceOf[Key[GBMModel]]
    val gbm = new GBM(params, key)
     model = gbm.trainModel.get

    //    val periodic = model._output._variable_importances.getRowHeaders()(0)
    params._ntrees = 40
    params._max_depth = 3

    val predict = model.score(toPredict, s"__${kpi}_prediction_only__")
    val predicted = Helper.sum2Vecs(train.vec(kpi), predict.lastVec)

    val predFrame: Key[Frame] = Key.make(s"${kpi}_predicted").asInstanceOf[Key[Frame]]
    val pp = new Frame(predFrame)
    pp.add("prediction", predicted)

    val predH2OFrame = H2OFrame(pp)
    //    predH2OFrame..add("prediction",predicted)

    val diff = difference(Helper.sum2Vecs(train.vec(kpi), toPredict.vec(kpi)), predicted)
    //  obj.kpiStats(kpi).prediction = predicted

    diff
  }


  def difference(org: Vec, pred: Vec): Vec = {
    val diff = Vec.makeZero(org.length)
    val diffW = diff.open()
    for (i <- 0L until org.length) {
      diffW.set(i, Math.abs(org.at(i) - pred.at(i)))
    }
    diffW.close()
    diff
  }




  def getAnomalies(vec: Vec, anomalies: Array[Int]): H2OFrame = {
    val va = Vec.makeZero(vec.length)
    val vaW = va.open()
    for (i <- 0L until vec.length) {
      if (anomalies.contains(i)) vaW.set(i, vec.at(i)) else vaW.setNA(i)
    }
    vaW.close()

    val key = Key.make[Frame](s"AD_points_$kpi")
    val out = new Frame(key, Array("anomalies"), Array(va))
    new H2OFrame(out)
  }
  
}
