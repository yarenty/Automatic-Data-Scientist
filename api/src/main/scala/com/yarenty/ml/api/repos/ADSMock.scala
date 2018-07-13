package com.yarenty.ml.api.repos

import com.yarenty.ml.api.types._

object ADSMock {

  def adsFlow(id:Int) = ADSFlow(
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
