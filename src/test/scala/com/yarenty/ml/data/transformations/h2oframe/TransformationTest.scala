package com.yarenty.ml.data.transformations.h2oframe

import java.net.URI

import com.yarenty.ml.data.transformations.{Cbrt, Ln, Root, Sqrt}
import com.yarenty.testhelpers.{AllH2OTests, TestHelper}
import org.junit.{After, BeforeClass, Test}
import water.fvec.{Frame, H2OFrame}

object TransformationTest extends AllH2OTests {

  val debug = false
  val kpiName = "90000000_"
  val file = TestHelper.getFileName("boxcox_test.csv")
  var dataFrame: Frame = _

  @BeforeClass
  def start(): Unit = {
    dataFrame = new H2OFrame(parser, new URI(file)) // parse input file

  }
}

class TransformationTest {

  private var test_fr: Frame = null
  private var expected_fr: Frame = null

  @Test
  def testLnTransform(): Unit = {
    initFrames(TransformationTest.dataFrame)
    Ln.transform(test_fr)
    // Create expected result
    for (c <- 0 until expected_fr.numCols) {
      val v = expected_fr.vec(c)
      for (r <- 0 until expected_fr.numRows.toInt) {
        v.set(r, Math.log(v.at(r)))
      }
    }
    TestHelper.assertEqualFrames(expected_fr, test_fr, 0.0001)
  }

  private def initFrames(fr: Frame): Unit = {
    test_fr = fr.deepCopy(null)
    expected_fr = fr.deepCopy(null)
  }

  @Test
  def testRootTransform(): Unit = {
    initFrames(TransformationTest.dataFrame)
    Root(5).transform(test_fr)
    for (c <- 0 until expected_fr.numCols) {
      val v = expected_fr.vec(c)
      for (r <- 0 until expected_fr.numRows.toInt) {
        v.set(r, Math.pow(v.at(r), 0.2))

      }

    }
    TestHelper.assertEqualFrames(expected_fr, test_fr, 0.0001)
  }

  @Test
  def testSqrtTransform(): Unit = {
    initFrames(TransformationTest.dataFrame)
    Sqrt.transform(test_fr)
    for (c <- 0 until expected_fr.numCols) {
      val v = expected_fr.vec(c)
      for (r <- 0 until expected_fr.numRows.toInt) {
        v.set(r, Math.sqrt(v.at(r)))

      }

    }
    TestHelper.assertEqualFrames(expected_fr, test_fr, 0.0001)
  }

  @Test
  //  @Parameters(method = "data")
  def testCbrtTransform(): Unit = {
    initFrames(TransformationTest.dataFrame)

    Cbrt.transform(test_fr)
    for (c <- 0 until expected_fr.numCols) {
      val v = expected_fr.vec(c)
      for (r <- 0 until expected_fr.numRows.toInt) {
        v.set(r, Math.cbrt(v.at(r)))
      }
    }
    TestHelper.assertEqualFrames(expected_fr, test_fr, 0.0001)
  }

  @After def clearFrame(): Unit = {
    test_fr.delete
    expected_fr.delete
  }

}
