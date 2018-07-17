package com.yarenty.ml.utils.h2o

import java.io._
import java.nio.charset.StandardCharsets

import hex.FrameSplitter
import org.apache.commons.io.FilenameUtils
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import water.Key
import water.fvec.{Frame, H2OFrame, Vec}
import water.parser.BufferedString
import water.util.FrameUtils.generateNumKeys
import water.util.Log

import scala.reflect.io.Directory

/**
  * (C)2018 by yarenty
  */
object Helper {

  def saveCSV(f: Frame, fileName: String): Unit = {
    Log.debug("CSV export::" + fileName)
    createOutputDirectory(fileName, true)
    //    if (session.storage == "hdfs") saveHDFSCSV(f, fileName)
    //    else {
    val csv = f.toCSV(true, false)
    val csv_writer = new PrintWriter(new File(fileName))
    while (csv.available() > 0) {
      csv_writer.write(csv.read.toChar)
    }
    csv_writer.close()
  }

  def createOutputDirectory(fileName: String, force: Boolean = false): Boolean = {
    val dir = FilenameUtils.getFullPathNoEndSeparator(fileName)
    Log.debug(s"Create output directory: $dir")
    val out = Directory(dir)
    out.createDirectory(force = force)
    if (force && !out.exists) {
      Log.err(s"Could not create output directory: $dir")
      System.exit(-1)
    }
    out.exists
  }

  def saveString(f: String, fileName: String, force: Boolean = true): Unit = {
    createOutputDirectory(fileName, force)
    val stream: InputStream = new ByteArrayInputStream(f.getBytes(StandardCharsets.UTF_8))
    val string_writer = new PrintWriter(new File(fileName))
    while (stream.available() > 0) {
      string_writer.write(stream.read.toChar)
    }
    string_writer.close()
  }


  //  def createOutputDirectory( fileName: String)(implicit session: Session): Unit = {
  //    import org.apache.hadoop.conf.Configuration
  //    import org.apache.hadoop.fs.FileSystem
  //    import org.apache.hadoop.fs.Path
  //
  //    val conf = new Configuration()
  //    conf.set("fs.default", session.storage_prefix)
  //    val fs = FileSystem.get(conf)
  //    val out = fs.create(new Path(fileName))
  //
  //  }



  def explodeCell(s: String): Array[String] = {
    val ss = s.split(",")
    ss.foreach(println) // scalastyle:ignore
    val bb = ss.map(c => c.split("=")(1))
    bb.foreach(println) // scalastyle:ignore
    bb
  }


  def tstName(s: String): Unit = {
    var found = false
    val c = s.split(",")
    found = false
    for (cc <- c) {
      val b = cc.split("=")
      if (b(0).trim == "Cell Name") {
        found = true
      }
    }
  }


  def vecCatToStringArray(v: Vec): Array[String] = {
    val arr = Array.ofDim[String](v.length.toInt)
    for (i <- 0 until v.length.toInt) {
      arr(i) = v.domain()(v.at8(i).toInt)
    }
    arr
  }

  def vecToStringArray(v: Vec): Array[String] = {
    val arr = Array.ofDim[String](v.length.toInt)
    val str = new BufferedString
    for (i <- 0 until v.length.toInt) {
      v.atStr(str, i)
      arr(i) = str.toString
    }
    arr
  }

  def vecToArrayNoNAN(v: Vec): Array[Double] = {
    val len = v.length - v.naCnt
    val arr = Array.ofDim[Double](len.toInt)
    var id = 0
    for (i <- 0 until v.length.toInt) {
      if (!v.isNA(i)) {
        arr(id) = v.at(i)
        id += 1
      }
    }
    arr
  }

  def arrayToVec(arr: Array[Double]): Vec = {
    val v = Vec.makeZero(arr.length)
    for (i <- arr.indices) {
      v.set(i, arr(i))
    }
    v
  }

  def arrayToTimeVec(arr: Array[Long]): Vec = {
    val vec = Vec.makeZero(arr.length, Vec.T_TIME)
    val vw = vec.open

    for (i <- arr.indices) {
      vw.set(i, arr(i))
    }
    vw.close()
    vec
  }

  def splitFrame(in: H2OFrame, ratio: Double): (Frame, Frame) = {
    val ratios = Array[Double](ratio)
    val o = Helper.split(in, ratios)
    (o(0), o(1))
  }

  def split[T <: Frame](fr: T, ratios: Seq[Double]): Array[Frame] = {
    val ks: Array[Key[Frame]] = generateNumKeys(fr._key, ratios.length + 1).map(_.asInstanceOf[Key[Frame]])
    val splitter = new FrameSplitter(fr, ratios.toArray, ks, null) //scalastyle:ignore
    water.H2O.submitTask(splitter)
    // return results
    splitter.getResult
  }

  def sum2Vecs(x: Vec, y: Vec): Vec = {
    val out = Vec.makeZero(x.length + y.length)
    val outW = out.open()
    for (i <- 0L until x.length) {
      outW.set(i, x.at(i))
    }
    for (i <- 0L until y.length) {
      outW.set(x.length + i, y.at(i))
    }
    outW.close()
    out
  }

  def cleanPredictions(x: Vec, min: Double = 0.0): Vec = {
    val out = Vec.makeZero(x.length)
    val outW = out.open()
    for (i <- 0L until x.length) {
      val v = x.at(i)
      if (v.isNaN) outW.set(i, v) else {
        if (v < min) outW.set(i, min) else outW.set(i, v)
      }
    }
    outW.close()
    out
  }

  def vecsToArrays(vecs: Array[Vec]): Array[Array[Double]] = {
    var res = Array.empty[Array[Double]]
    for (vec <- vecs) {
      val oneA = vecToArray(vec)
      res = res :+ oneA
    }
    res
  }

  def vecToArray(v: Vec): Array[Double] = {
    val arr = Array.ofDim[Double](v.length.toInt)
    for (i <- 0 until v.length.toInt) {
      arr(i) = v.at(i)
    }
    arr
  }


  def vecToDateArray(v: Vec): Array[Long] = {
    val arr = Array.ofDim[Long](v.length.toInt)
    for (i <- 0 until v.length.toInt) {
      arr(i) = v.at8(i)
    }
    arr
  }


  def idx2Arr(idxs: Array[Int], kpi: Array[Double]): Array[Double] = {
    if (idxs.isEmpty) {
      Array.empty
    }
    else {
      val o = Array.ofDim[Double](kpi.length)
      for (x <- o.indices) o(x) = Double.NaN
      for (i <- idxs) {
        o(i) = kpi(i)
      }
      o
    }
  }




}
