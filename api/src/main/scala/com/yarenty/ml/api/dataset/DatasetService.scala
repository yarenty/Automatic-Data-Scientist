package com.yarenty.ml.api.dataset

import java.net.URI

import com.yarenty.io.FileUtils
import com.yarenty.ml.preprocessing.{EmptyCSVParser, NameRegularization}
import water.fvec.H2OFrame
import water.util.Log


object DatasetService {


  val path: String = "/opt/data/demo/"
  
  def getList() = FileUtils.getListOfDirs(path)

  def loadData(ds: String): H2OFrame = {
    
    Log.info(s"Processing dataset:${ds}")

    val f = path + ds + "/data.csv"
    
    if (FileUtils.fileExist(f)) {

      val _tFP1 = System.nanoTime()

      //    val parser = if (session.NANprocess) EmptyCSVParser.get.setNAStrings(session.NANStrings) else EmptyCSVParser.get
      val parser = EmptyCSVParser.get

      val in = new H2OFrame(parser, new URI(f))
      val _timeFP = System.nanoTime() - _tFP1

      Log.info(s"TIME for processing file: ${_timeFP} (${_timeFP / 1000000000}sec)")
      val data = NameRegularization(in)
      //    val nodeData = TimeRegularization(nodeData2)
      //if (session.debugMode) _save(nodeData)

      //    if (session.NANprocess) NANProcessor(nodeData)
      data.remove(Array("C1"))
      
      data
    } else {
      null
    }
  }
  
  
  
  
  
  /* TEST ! */

  def main(args: Array[String]): Unit = {
    
    println("get DS list")
    println(getList().mkString("; "))

    println("wrong load:")
    println(loadData("Aa"))

    println("load:")
    println(loadData("ClusterKPI48"))

    println("????")
  }

}