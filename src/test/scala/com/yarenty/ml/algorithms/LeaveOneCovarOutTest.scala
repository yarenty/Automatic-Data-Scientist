package com.yarenty.ml.algorithms

import java.net.URI

import com.yarenty.testhelpers.{AllH2OTests, TestHelper}
import hex.genmodel.utils.DistributionFamily
import hex.genmodel.utils.DistributionFamily._
import hex.tree.gbm.{GBM, GBMModel}
import org.junit.{BeforeClass, Test}
import water.DKV
import water.fvec.H2OFrame


object LeaveOneCovarOutTest extends AllH2OTests {


  var prostateFrame: H2OFrame = _
  var carFrame: H2OFrame = _

  // Init cloud before run
  @BeforeClass
  def start(): Unit = {
    prostateFrame = getFrame(TestHelper.getFileName("prostate.csv"))
    carFrame = getFrame(TestHelper.getFileName("cars.csv"))

  }

  def getFrame(fname: String): H2OFrame = {
    new H2OFrame(TestHelper.getSimpleCSVParser, new URI("file://" + fname))
  }

}

/**
  * This Junit is mainly used to detect leaks in Leave One Covariate Out (LOCO)
  */
// scalastyle:off
class LeaveOneCovarOutTest {
  val T = LeaveOneCovarOutTest

  @Test def testLocoRegressionDefault(): Unit = { //Regression case
    hackIfNeeded(T.carFrame, "economy (mpg)", gaussian)
    val gbm = buildGBM(T.carFrame, "economy (mpg)", gaussian)

    val loco = LeaveOneCovarOut.leaveOneCovarOut(gbm, T.carFrame, null, null)
    assert(DKV.get(loco._key) != null, "LOCO frame with default transform is not in DKV!")

  }

  @Test def testLocoBernoulliDefault(): Unit = { //Bernoulli case
    hackIfNeeded(T.prostateFrame, "CAPSULE", bernoulli)
    val gbm = buildGBM(T.prostateFrame, "CAPSULE", bernoulli)

    val loco = LeaveOneCovarOut.leaveOneCovarOut(gbm, T.prostateFrame, null, null)
    assert(DKV.get(loco._key) != null, "LOCO frame with default transform is not in DKV!")
  }

  @Test def testLocoMultinomialDefault(): Unit = { //Multinomial case
    hackIfNeeded(T.carFrame, "cylinders", multinomial)
    val gbm = buildGBM(T.carFrame, "cylinders", multinomial)
    val loco = LeaveOneCovarOut.leaveOneCovarOut(gbm, T.carFrame, null, null)
    assert(DKV.get(loco._key) != null, "LOCO frame with default transform is not in DKV!")
  }

  def hackIfNeeded(fr: H2OFrame, response: String, family: DistributionFamily) = {
    var idx = fr.find(response)
    if ((family eq DistributionFamily.bernoulli) || (family eq DistributionFamily.multinomial) || (family eq DistributionFamily.modified_huber)) {
      if (!fr.vec(idx).isCategorical) {
        fr.replace(idx, fr.vec(idx).toCategoricalVec)
      }
    }
    DKV.put(fr) // Update frame after hacking it

  }

  def buildGBM(fr: H2OFrame, response: String, family: DistributionFamily): GBMModel = {
    val parms = new GBMModel.GBMParameters
    parms._train = fr._key
    parms._response_column = response
    parms._ntrees = 5
    parms._distribution = family
    parms._max_depth = 4
    parms._min_rows = 1
    parms._nbins = 50
    parms._learn_rate = .2f
    parms._score_each_iteration = true

    val job = new GBM(parms)
    val gbm = job.trainModel.get
    gbm
  }

  @Test def testLocoRegressionMean(): Unit = {
    hackIfNeeded(T.carFrame, "economy (mpg)", gaussian)
    val gbm = buildGBM(T.carFrame, "economy (mpg)", gaussian)

    val loco = LeaveOneCovarOut.leaveOneCovarOut(gbm, T.carFrame, "mean", null)
    assert(DKV.get(loco._key) != null, "LOCO frame with mean transform is not in DKV!")
  }

  @Test def testLocoBernoulliMean(): Unit = {
    hackIfNeeded(T.prostateFrame, "CAPSULE", bernoulli)
    val gbm = buildGBM(T.prostateFrame, "CAPSULE", bernoulli)
    val loco = LeaveOneCovarOut.leaveOneCovarOut(gbm, T.prostateFrame, "mean", null)
    assert(DKV.get(loco._key) != null, "LOCO frame with mean transform is not in DKV!")
  }

  @Test def testLocoMultinomialMean(): Unit = {
    hackIfNeeded(T.carFrame, "cylinders", multinomial)
    val gbm = buildGBM(T.carFrame, "cylinders", multinomial)
    val loco = LeaveOneCovarOut.leaveOneCovarOut(gbm, T.carFrame, "mean", null)
    assert(DKV.get(loco._key) != null, "LOCO frame with default transform is not in DKV!")
  }

  @Test def testLocoRegressionMedian(): Unit = {
    hackIfNeeded(T.carFrame, "economy (mpg)", gaussian)
    val gbm = buildGBM(T.carFrame, "economy (mpg)", gaussian)

    val loco = LeaveOneCovarOut.leaveOneCovarOut(gbm, T.carFrame, "median", null)
    assert(DKV.get(loco._key) != null, "LOCO frame with mean transform is not in DKV!")
  }

  @Test def testLocoBernoulliMedian(): Unit = {
    hackIfNeeded(T.prostateFrame, "CAPSULE", bernoulli)
    val gbm = buildGBM(T.prostateFrame, "CAPSULE", bernoulli)
    val loco = LeaveOneCovarOut.leaveOneCovarOut(gbm, T.prostateFrame, "median", null)
    assert(DKV.get(loco._key) != null, "LOCO frame with median transform is not in DKV!")
  }

  @Test def testLocoMultinomialMedian(): Unit = {
    hackIfNeeded(T.carFrame, "cylinders", multinomial)
    val gbm = buildGBM(T.carFrame, "cylinders", multinomial)
    val loco = LeaveOneCovarOut.leaveOneCovarOut(gbm, T.carFrame, "median", null)
    assert(DKV.get(loco._key) != null, "LOCO frame with default transform is not in DKV!")
  }
}


// scalastyle:on