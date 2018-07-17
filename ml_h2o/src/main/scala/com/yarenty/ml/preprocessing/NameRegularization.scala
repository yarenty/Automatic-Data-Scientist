package com.yarenty.ml.preprocessing

import water.fvec.H2OFrame
import water.util.Log

object NameRegularization {

  def apply(data: H2OFrame): H2OFrame = {
    Log.info("Regularize column name")
    Log.debug(data._names.toList)

    //renaming as using . in names
    for (name <- data.names()) {
      val n = name.replace(".", "_")
      data.rename(name, n)
    }
    
    Log.debug(data._names.toList)
    data
  }
}
