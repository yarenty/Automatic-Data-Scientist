package com.yarenty.ml.data.transformations.movingaverage

import water.fvec.Frame


package object h2oframe {

  trait MovingAverage {
    def average(data: Frame): Frame
  }

}
