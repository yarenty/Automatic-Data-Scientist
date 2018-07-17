package com.yarenty.ml.api.repos

import com.yarenty.ml.api.types._

object ADSMock {

  def adsFlow(output:String) = ADSFlow(
    "kpi_AD",
    1, //id
    "DONE", //RUNING, STARTED, CANCELED, DONE
    Dataset("ClusterKPI48",
      "",
      ""
    ),
    AFE(1,
      true,
      List(
        Transformation("DayOfWeek"),
        Transformation("HourOfDay"),
        Transformation("MinuteOfDay")
      )
    ),
    Algorithms(
      false,
      "RMSE",
      List(
        Algorithm(
          "GBM",
          List("ntrees=40", "min_depth=3", "max_depth=3", "distribution=gaussian")
        )
      )
    ),
    "LOCO", //lime/loco
    Some(false),
    output
  )
  
  
  
}
