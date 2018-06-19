package com.yarenty.ml.data.transformations

import water.fvec.{Frame, Vec}


trait TransformType {
  def transform(fr: Frame): Frame = {
    for (v <- fr.vecs) transform(v)
    fr
  }

  def transform(vec: Vec): Vec = {
    for (i <- 0 until vec.length.toInt) {
      vec.set(i, transform(vec.at(i)))
    }
    vec
  }

  def transform(d: Double): Double

}

case object Ln extends TransformType {
  override def transform(d: Double): Double = Math.log(d)
}

case object Log10 extends TransformType {
  override def transform(d: Double): Double = Math.log10(d)
}


case object Sqrt extends TransformType {
  override def transform(d: Double): Double = Math.sqrt(d)
}

case object Cbrt extends TransformType {
  override def transform(d: Double): Double = Math.cbrt(d)
}

case class Root(root: Double) extends TransformType {
  override def transform(d: Double): Double = Math.pow(d, 1.0 / root)
}

case class Pow(pow: Double) extends TransformType {
  override def transform(d: Double): Double = Math.pow(d, pow)
}


case class Lambda(lambda: Double) extends TransformType {
  override def transform(d: Double): Double = (Math.pow(d, lambda) - 1.0) / lambda
}


package object h2oframe {

  def log(vec: Vec): Vec = Ln.transform(vec)

  def log10(vec: Vec): Vec = Log10.transform(vec)

  def sqrt(vec: Vec): Vec = Sqrt.transform(vec)

  def cbrt(vec: Vec): Vec = Cbrt.transform(vec)

  def root(vec: Vec, root: Double): Vec = Root(root).transform(vec)

  def pow(vec: Vec, pow: Double): Vec = Pow(pow).transform(vec)
}
