package com.yarenty.ml.data.transformations


package object movingaverage {

  trait MovingAverage {
    def average(data: Array[Double]): Array[Double]
  }


}

