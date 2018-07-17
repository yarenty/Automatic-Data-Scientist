package com.yarenty.ml.preprocessing

import water.fvec.Vec
import water.util.Log

/**
  * Created by yarenty on 05/12/2016.
  */
object KPIProcessor {
  
  /**
    * Temp for demo
    */
  def apply(v1: Vec, v2: Vec): Vec = {

    Log.info("KPI  processing ..... ")
    val out = Vec.makeZero(v1.length)
    
    val outW = out.open()
    for (i <- 0L until v1.length) {
      val dt = v1.at(i) / v2.at(i)
      outW.set(i, dt)
    }
    outW.close()

    out
  }
}

