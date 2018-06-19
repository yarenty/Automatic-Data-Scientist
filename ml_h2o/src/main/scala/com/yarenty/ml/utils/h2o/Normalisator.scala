package com.yarenty.ml.utils.h2o

import water.Key
import water.fvec.{Frame, H2OFrame, Vec}

/**
  * (C)2018 by yarenty
  */

sealed abstract class NormalisatorType

case object MinMaxNormalisatorType extends NormalisatorType

case object ZeroToOneNormalisatorType extends NormalisatorType


object Normalisator {
  val normalisatorType: NormalisatorType = MinMaxNormalisatorType

  def apply(in: H2OFrame, name: String, ignored: Array[String]): H2OFrame = {
    val key: Key[Frame] = Key.make(name + "_normalized").asInstanceOf[Key[Frame]]
    val out = new H2OFrame(new Frame(key))

    for (n <- in.names) {
      if (!ignored.contains(n)) {
        val v = in.vec(n)
        if (v.get_type() == Vec.T_NUM) {
          val o: Vec = normalisatorType match {
            case MinMaxNormalisatorType => minMax(v)
            case ZeroToOneNormalisatorType => zeroToOne(v)
          }
          out.add(n, o)
        } else {
          out.add(n, v)
        }
      }
    }
    out
  }

  private def minMax(v: Vec) = { //0 to 1
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

  private def zeroToOne(v: Vec) = { //0 to 1
    if (v.max <= 1.0 && v.min >= 0.0) v //already there ...
    else {
      val o = Vec.makeZero(v.length)
      val oW = o.open()
      val min = if (v.min() < 0) v.min() else 0.0
      val max = v.max - min
      for (i <- 0L until v.length) {
        oW.set(i, (v.at(i) + min) / max)
      }
      oW.close()
      o
    }
  }

}
