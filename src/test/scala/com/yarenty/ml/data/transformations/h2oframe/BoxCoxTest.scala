package com.yarenty.ml.data.transformations.h2oframe

import java.net.URI

import com.yarenty.testhelpers.{AllH2OTests, TestHelper}
import org.junit.Assert.assertEquals
import org.junit.{BeforeClass, Test}
import water.Key
import water.fvec.{Frame, H2OFrame}
import water.parser.ParseDataset


object BoxCoxTest extends AllH2OTests {

  val debug = false
  val kpiName = "90000000_"
  val file = TestHelper.getFileName("boxcox_test.csv")

  var dataFrame: Frame = _

  @BeforeClass
  def start(): Unit = {
    dataFrame = new H2OFrame(parser, new URI(file)) // parse input file
  }
}


class BoxCoxTest {

  @Test
  def testBoxCox(): Unit = {
    val test_fr = BoxCox.transform(BoxCoxTest.dataFrame.deepCopy(null), Array[Double](1.4))
    val raw = Key.make[Frame]("bc_key")
    val expected = Key.make("exp_key")
    TestHelper.makeByteVec(raw, "4145.576\n4337.138\n5254.264\n4839.082\n5954.205\n7257.936\n7216.094\n5343.578\n4801.825")
    val exp = ParseDataset.parse(expected, raw)

    for (i <- 0 until test_fr.numCols) {
      for (j <- 0 until test_fr.numRows.toInt) {
        assertEquals(test_fr.vec(i).at(j), exp.vec(i).at(j), 0.01)
      }
    }
    test_fr.delete()
  }

  @Test def testLambdaSearch(): Unit = {
    val ls = BoxCox.lambdaSearch(BoxCoxTest.dataFrame)
    for (i <- 0 until ls.length) {
      assertEquals(-0.9999242, ls(i), 0.01)

    }
  }


}
