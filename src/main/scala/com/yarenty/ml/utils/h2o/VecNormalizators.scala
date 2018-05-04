package com.yarenty.ml.utils.h2o

import breeze.numerics._
import water.fvec.Vec


// scalastyle:off
object VecNormalizators {

  def MinMax(v: Vec): Vec = { //0 to 1
    if (v.max <= 1.0 && v.min >= 0.0) v //already there ...
    else {
      val o = Vec.makeZero(v.length)
      val oW = o.open()
      val max = v.max() - v.min()
      val min = v.min
      for (i <- 0L until v.length) {
        oW.set(i, (v.at(i) - min) / max)
      }
      oW.close()
      o
    }
  }


  def ZeroToOne(v: Vec) = { //0 to 1
    if (v.max <= 1.0 && v.min >= 0.0) v //already there ...
    else {
      val o = Vec.makeZero(v.length)
      val oW = o.open()
      val max = v.max()
      for (i <- 0L until v.length) {
        oW.set(i, v.at(i) / max)
      }
      oW.close()
      o
    }
  }


  def Log(v: Vec, l: Double) = { //0 to 1
    val o = Vec.makeZero(v.length)
    val oW = o.open()

    for (i <- 0L until v.length) {
      oW.set(i, log(l, v.at(i)))
    }
    oW.close()
    o
  }

  def Log10(v: Vec) = Log(v, 10.0)

  def Log100(v: Vec) = Log(v, 100.0)

  def Log1000(v: Vec) = Log(v, 1000.0)


  def Pow(v: Vec, p: Double) = { //0 to 1
    val o = Vec.makeZero(v.length)
    val oW = o.open()

    for (i <- 0L until v.length) {
      oW.set(i, pow(v.at(i), p))
    }
    oW.close()
    o
  }

  def Sqrt(v: Vec) = { //0 to 1
    val o = Vec.makeZero(v.length)
    val oW = o.open()

    for (i <- 0L until v.length) {
      oW.set(i, sqrt(v.at(i)))
    }
    oW.close()
    o
  }


  def Sinc(v: Vec, p: Double) = { //0 to 1
    val o = Vec.makeZero(v.length)
    val oW = o.open()

    for (i <- 0L until v.length) {
      oW.set(i, sinc(v.at(i)): Double)
    }
    oW.close()
    o
  }

  def SincPi(v: Vec, p: Double) = { //0 to 1
    val o = Vec.makeZero(v.length)
    val oW = o.open()

    for (i <- 0L until v.length) {
      oW.set(i, sincpi(v.at(i)): Double)
    }
    oW.close()
    o
  }
}

// scalastyle:on
