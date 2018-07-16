package com.yarenty.ml.api.repos

import com.yarenty.ml.api.types._

object ADSMock {

  def adsFlow(id:Int) = ADSFlow(
    "TEst ADS flow",
    3, //id
    "RUNING", //RUNING, STARTED, CANCELED, DONE
    Dataset("train",
      "test",
      "valid"
    ),
    AFE(id,
      true,
      List(
        Transformation(
          "aaa",
          List("A", "B")
        )
      )
    ),
    Algorithms(
      false,
      "LogLoss",
      List(

        Algorithm(
          "GBM",
          List("A", "fsdvdf")
        )
      )
    ),
    "LOCO", //lime/loco
    Some(false)
  )
  
  
  
}
