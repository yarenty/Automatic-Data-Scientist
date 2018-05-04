package com.yarenty.ml.algorithms

import hex.quantile.QuantileModel.CombineMethod
import hex.{Model, ModelCategory}
import water._
import water.exceptions.H2OIllegalArgumentException
import water.fvec.{Chunk, Frame, NewChunk, Vec}
import water.rapids.ast.prims.reducers.AstMedian
import water.util.Log


/**
  * Leave One Covariate Out (LOCO)
  *
  * Calculates row-wise variable importance's by re-scoring a trained supervised model and measuring the impact of setting
  * each variable to missing or it’s most central value(mean or median & mode for categorical's)
  *
  * (C)2018 by yarenty
  */
object LeaveOneCovarOut extends Iced {

  /**
    * Conduct Leave One Covariate Out (LOCO) given a model, frame, job, and replacement value
    *
    * @param m          Supervised H2O model
    * @param fr         H2O Frame to score
    * @param replaceVal Value to replace column by when conducting LOCO ("mean" or "median"). Default behavior is to setting to NA
    * @param frameKey   Key of final Leave One Covariate Out(LOCO) Frame.
    * @return An H2OFrame displaying the base prediction (model scored with all predictors) and the difference in predictions
    *         when variables are dropped/replaced. The difference displayed is the base prediction substracted from
    *         the new prediction (when a variable is dropped/replaced with mean/median/mode) for binomial classification
    *         and regression problems. For multinomial problems, the sum of the absolute value of differences across classes
    *         is calculated per column dropped/replaced.
    */
  def leaveOneCovarOut(m: Model[_, _ <: Model.Parameters, _ <: Model.Output], fr: Frame, replaceVal: String, frameKey: Key[Frame]): Frame = {
    val locoAnalysisFrame = new Frame //Set up initial LOCO frame

    if (m._output.getModelCategory ne ModelCategory.Multinomial) {
      locoAnalysisFrame.add("base_pred", getBasepredictions(m, fr)(0))
      //If not multinomial, then predictions are one column


    } else {
      locoAnalysisFrame.add(new Frame(null, getBasepredictions(m, fr)))
      locoAnalysisFrame._names(0) = "base_pred"
    }


    val predictors = m._output._names
    //Get predictors

    val tasks = Array.ofDim[Vec](predictors.length - 1, 2)

    val start = System.currentTimeMillis
    Log.info("Starting Leave One Covariate Out (LOCO) analysis for model " + m._key + " and frame " + fr._key)

    for (i <- 0 until predictors.length - 1) {
      tasks(i) = compute(locoAnalysisFrame, fr, m, predictors(i), replaceVal)
    }


    //If multinomial, then we need to remove predicted probabilities for each class. We only want the final class predicted as the first column
    if (m._output.getModelCategory eq ModelCategory.Multinomial) {
      val colsToRemove = new Array[Int](locoAnalysisFrame.numCols - 1)
      for (i <- colsToRemove.indices) {
        colsToRemove(i) = i + 1
      }
      locoAnalysisFrame.remove(colsToRemove)
    }

    for (i <- tasks.indices) {
      locoAnalysisFrame.add("rc_" + predictors(i), tasks(i)(0))
      Log.info(s" ${predictors(i)}: ${tasks(i)(0).max}  ${tasks(i)(0).mean} ${tasks(i)(0).min}")
    }


    Log.info("Finished Leave One Covariate Out (LOCO) analysis for model " + m._key + " and frame " + fr._key + " in " + (System.currentTimeMillis - start) / 1000.0 +
      " seconds for " + (predictors.length - 1) + " columns"
    )

    //Put final frame into DKV
    if (frameKey != null) {
      locoAnalysisFrame._key = frameKey
      DKV.put(locoAnalysisFrame._key, locoAnalysisFrame)
    }
    else {
      locoAnalysisFrame._key = Key.make("loco_" + fr._key.toString + "_" + m._key.toString)
      DKV.put(locoAnalysisFrame._key, locoAnalysisFrame)
    }
    locoAnalysisFrame
  }


  def compute(_locoFrame: Frame, _frame: Frame,
              _model: Model[_, _ <: Model.Parameters, _ <: Model.Output],
              _predictor: String, _replaceVal: String): Array[Vec] = {

    var _result: Array[Vec] = null

    if (_model._output.getModelCategory eq ModelCategory.Multinomial) {
      val predTmp = getNewPredictions(_model, _frame, _predictor, _replaceVal)
      val tmpFrame = new Frame().add(_locoFrame).add(new Frame(null, predTmp))
      _result = new LeaveOneCovarOut.MultiDiffTask(_model._output.nclasses).doAll(Vec.T_NUM, tmpFrame).outputFrame.vecs
      for (v <- predTmp) {
        v.remove() //Clean up DKV, otherwise we will get leaked keys
      }
    }
    else {
      _result = getNewPredictions(_model, _frame, _predictor, _replaceVal)
      new LeaveOneCovarOut.DiffTask().doAll(_locoFrame.vec(0), _result(0))
    }
    Log.info("Completed Leave One Covariate Out (LOCO) Analysis for column: " + _predictor)

    _result
  }

  /**
    * Get new predictions based on dropping/replacing a column with mean, median, or mode
    *
    * @param m            An H2O supervised model
    * @param fr           A Frame to score on
    * @param colToDrop    Column to modify/drop before prediction
    * @param valToReplace Value to replace colToDrop by (Default is null)
    * @return
    */
  private def getNewPredictions(m: Model[_, _ <: Model.Parameters, _ <: Model.Output], fr: Frame, colToDrop: String, valToReplace: String): Array[Vec] = {
    val workerFrame = new Frame(fr)
    val vecToReplace = fr.vec(colToDrop)
    var replacementVec: Vec = Vec.makeCon(0.0, 0)
    if (valToReplace == null) replacementVec = vecToReplace.makeCon(Double.NaN)
    else if (valToReplace == "mean") if (vecToReplace.isCategorical) {
      val tmpVec = vecToReplace.makeCon(vecToReplace.mode)
      replacementVec = tmpVec.toCategoricalVec //Can only get mode for categoricals

      tmpVec.remove()
    }
    else replacementVec = vecToReplace.makeCon(vecToReplace.mean)
    else if (valToReplace == "median") if (vecToReplace.isCategorical) {
      val tmpVec = vecToReplace.makeCon(vecToReplace.mode)
      replacementVec = tmpVec.toCategoricalVec
      tmpVec.remove()
    }
    else {
      val tmpFr = new Frame(vecToReplace)
      val median = AstMedian.median(tmpFr, CombineMethod.AVERAGE)
      replacementVec = vecToReplace.makeCon(median)
    }
    else throw new H2OIllegalArgumentException("Invalid value to replace columns in LOCO. Got " + valToReplace)
    val vecToDropIdx = fr.find(colToDrop)
    workerFrame.replace(vecToDropIdx, replacementVec)
    DKV.put(workerFrame)
    val modifiedPredictionsFr = m.score(workerFrame, null, null, false)
    try
        if (m._output.getModelCategory eq ModelCategory.Binomial) {
          val modifiedPrediction = modifiedPredictionsFr.remove(2)
          modifiedPredictionsFr.delete()
          Array[Vec](modifiedPrediction)
        }
        else if (m._output.getModelCategory eq ModelCategory.Multinomial) {
          val vecs = modifiedPredictionsFr.vecs
          DKV.remove(modifiedPredictionsFr._key)
          vecs
        }
        else {
          val modifiedPrediction = modifiedPredictionsFr.remove(0)
          modifiedPredictionsFr.delete()
          Array[Vec](modifiedPrediction)
        }
    finally {
      DKV.remove(workerFrame._key)
      replacementVec.remove()
    }
  }

  /**
    * Get base predictions given a model and frame. "Base predictions" are predictions based on all features in the
    * model
    *
    * @param m  An H2O supervised model
    * @param fr A Frame to score on
    * @return An array of Vecs containing predictions
    */
  private def getBasepredictions(m: Model[_, _ <: Model.Parameters, _ <: Model.Output], fr: Frame): Array[Vec] = {
    val basePredsFr = m.score(fr, null, null, false)
    if (m._output.getModelCategory eq ModelCategory.Binomial) {
      val basePreds = basePredsFr.remove(2)
      basePredsFr.delete()
      Array[Vec](basePreds)
    }
    else if (m._output.getModelCategory eq ModelCategory.Multinomial) {
      val basePredsVecs = basePredsFr.vecs
      DKV.remove(basePredsFr._key)
      basePredsVecs
    }
    else {
      val basePreds = basePredsFr.remove(0)
      basePredsFr.delete()
      Array[Vec](basePreds)
    }
  }

  private class DiffTask extends MRTask[LeaveOneCovarOut.DiffTask] {
    override def map(c: Array[Chunk]): Unit = {
      val _basePred = c(0)
      for (chnk <- 1 until c.length) {
        for (row <- 0 until c(0)._len) {
          c(chnk).set(row, c(chnk).atd(row) - _basePred.atd(row))
        }
      }
    }
  }

  private class MultiDiffTask(val _numClasses: Int) extends MRTask[LeaveOneCovarOut.MultiDiffTask] {

    override def map(cs: Array[Chunk], nc: NewChunk): Unit = {
      for (i <- 0 until cs(0)._len) {
        var d = 0.0
        for (j <- 1 to _numClasses) {
          val v = cs(j + _numClasses + 1).atd(i) - cs(j).atd(i)
          d += Math.abs(v)
        }
        nc.addNum(d)
      }
    }
  }

}
  


