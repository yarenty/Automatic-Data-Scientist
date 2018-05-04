package com.yarenty.testhelpers

import hex.FrameSplitter
import org.joda.time.format.DateTimeFormat
import org.joda.time.{DateTime, DateTimeZone}
import org.junit.Assert.assertEquals
import org.junit.Ignore
import water.fvec.{Frame, H2OFrame, Vec, _}
import water.parser.{DefaultParserProviders, ParseSetup}
import water.{Key, _}


class TestHelper {} // scalastyle:ignore - need class to find resources

@Ignore("Support for tests, but no actual tests here")
object TestHelper {

  def generateFileName(name: String): String = getFileName("prostate.csv").replace("prostate.csv", name)

  // scalastyle:off
  def getFileName(fileName: String): String = {
    var is = getClass.getResource(fileName)
    if (null == is) is = getClass.getClassLoader.getResource(fileName)
    if (null == is) classOf[TestHelper].getResource("/../resources/" + fileName)
    if (null == is) classOf[TestHelper].getResource("/../../resources/test/" + fileName)
    is.getPath
  }

  def vecToArray(v: Vec): Array[Double] = {
    val arr = Array.ofDim[Double](v.length.toInt)
    for (i <- 0 until v.length.toInt) {
      arr(i) = v.at(i)
    }
    arr
  }


  def arrayToVec(arr: Array[Double]): Vec = {
    val vec = Vec.makeZero(arr.length)
    val vw = vec.open

    for (i <- arr.indices) {
      vw.set(i, arr(i))
    }
    vw.close()
    vec
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

  //  def split(in: H2OFrame, ratio: Double): (Frame, Frame) = {
  def split(in: H2OFrame, ratio: Double): (H2OFrame, H2OFrame) = {

    val keys = Array[String]("train_" + ratio, "test" + ratio)
    val ratios = Array[Double](ratio)

    val frs = split(in, keys, ratios)
    //    (frs(0), frs(1))
    (H2OFrame(frs(0)), H2OFrame(frs(1)))
  }


  def split[T <: Frame](fr: T, keys: Seq[String], ratios: Seq[Double]): Array[Frame] = {
    val ks = keys.map(Key.make[Frame](_)).toArray
    val splitter = new FrameSplitter(fr, ratios.toArray, ks, null)
    water.H2O.submitTask(splitter)
    // return results
    splitter.getResult
  }


  def getSimpleCSVParser: ParseSetup = {
    val p = new ParseSetup()
    p.setParseType(DefaultParserProviders.CSV_INFO)
    p.setSeparator(44)
    p.setSingleQuotes(false)
    p.setCheckHeader(1)
    p
  }

  // scalastyle:on


  def dummyTimestamp(d: Array[Double]): Array[String] = {
    val out = Array.ofDim[String](d.length)
    val dtz = DateTimeZone.forID("UTC")
    val formatter = DateTimeFormat.forPattern("dd/MM/yyyy").withZone(dtz)
    val dfi = new DateTime()
    for (i <- 0 until d.length) {
      out(i) = formatter.print(dfi.plusDays(i))
    }
    out
  }

  def makeByteVec(k: Key[Frame], data: String*): Key[_ <: Keyed[_]] = {
    val chunks = new Array[Array[Byte]](data.length)
    val espc = new Array[Long](data.length + 1)
    for (i <- 0 until chunks.length) {
      chunks(i) = data(i).getBytes
      espc(i + 1) = espc(i) + data(i).length
    }
    val fs = new Futures
    val key = Vec.newKey
    val bv = new ByteVec(key, Vec.ESPC.rowLayout(key, espc))
    for (i <- 0 until chunks.length) {
      val chunkKey = bv.chunkKey(i)
      DKV.put(chunkKey, new Value(chunkKey, chunks(i).length, chunks(i), TypeMap.C1NCHUNK, Value.ICE), fs)
    }
    DKV.put(bv._key, bv, fs)
    val fr = new Frame(k, Array[String]("makeByteVec"), Array[Vec](bv))
    DKV.put(k, fr, fs)
    fs.blockForPending()
    k
  }


  def assertEqualFrames(expected: Frame, actual: Frame, threshold: Double): Unit = {
    assertEquals("Mismatched number of columns in Frame", expected.numCols, actual.numCols)
    for (c <- 0 until expected.numCols) {
      val v_e = expected.vec(c)
      val v_a = actual.vec(c)
      assertEquals("Mismatched length of Vec at column " + c, v_e.length, v_a.length)
      for (r <- 0 until v_e.length.toInt) {
        assertEquals("Mismatched value at [" + r + "," + c + "]", v_e.at(r), v_a.at(r), threshold)

      }
    }
  }


}
