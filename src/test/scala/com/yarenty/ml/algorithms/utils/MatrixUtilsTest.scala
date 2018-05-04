package com.yarenty.ml.algorithms.utils

import org.apache.commons.math3.linear.{Array2DRowRealMatrix, RealMatrix, SingularValueDecomposition}
import org.junit.{Assert, Test}


class MatrixUtilsTest {

  // scalastyle:off
  @Test def testAryule1k2(): Unit = {
    val k = 2
    val R = Array[Double](1, 2, 3)
    val A = Array.ofDim[Double](k + 1)
    MatrixUtils.aryule(R, A, k)
    Assert.assertEquals(1.3333333333333335d, A(1), 0.001d)
    Assert.assertEquals(0.3333333333333333d, A(2), 0.001d)
  }

  @Test def testAryule1k9(): Unit = {
    val k = 9
    val R = Array[Double](143.85, 141.95, 141.45, 142.30, 140.60, 140.00, 138.40, 137.10, 138.90, 139.85)
    val A = new Array[Double](k + 1)
    MatrixUtils.aryule(R, A, k)
    val expected = Array[Double](-1.0, 2.90380682, 0.31235631, 1.26463104, -3.30187384, -1.61653593, -2.10367317, 1.37563117, 2.18139823, 0.02314717)
    Assert.assertArrayEquals(expected, A, 0.01d)
  }

  @Test def testArburgk2(): Unit = { // Expected outputs are obtained from Octave's arburg() function
    // k must be less than (X.length - 2) on Octave, but A can be computed w/o the assumption
    val k = 2
    val X = Array[Double](1, 2, 3, 4, 5)
    val A = new Array[Double](k + 1)
    MatrixUtils.arburg(X, A, k)
    Assert.assertEquals(-1.86391d, A(1), 0.00001d)
    Assert.assertEquals(0.95710d, A(2), 0.00001d)
  }

  @Test def testArburgk5(): Unit = {
    val k = 5
    val X = Array[Double](143.85, 141.95, 141.45, 142.30, 140.60, 140.00, 138.40, 137.10, 138.90, 139.85)
    val A = new Array[Double](k + 1)
    MatrixUtils.arburg(X, A, k)
    val expected = Array[Double](-1.0, -1.31033, 0.58569, -0.56058, 0.63859, -0.35334)
    Assert.assertArrayEquals(expected, A, 0.00001d)
  }

  @Test def testToeplitz(): Unit = {
    val c = Array[RealMatrix](new Array2DRowRealMatrix(Array[Double](1)), new Array2DRowRealMatrix(Array[Double](2)), new Array2DRowRealMatrix(Array[Double](3)))
    val A = MatrixUtils.toeplitz(c, 3)
    // 1  2  3
    // 2  1  2
    // 3  2  1
    //    Assert.assertArrayEquals(Array[RealMatrix](new Array2DRowRealMatrix(Array[Double](1)), new Array2DRowRealMatrix(Array[Double](2)), new Array2DRowRealMatrix(Array[Double](3))), A(0))
    //    Assert.assertArrayEquals(Array[RealMatrix](new Array2DRowRealMatrix(Array[Double](2)), new Array2DRowRealMatrix(Array[Double](1)), new Array2DRowRealMatrix(Array[Double](2))), A(1))
    //    Assert.assertArrayEquals(Array[RealMatrix](new Array2DRowRealMatrix(Array[Double](3)), new Array2DRowRealMatrix(Array[Double](2)), new Array2DRowRealMatrix(Array[Double](1))), A(2))
  }

  @Test def testCombineMatrices1D(): Unit = {
    val m1 = Array[RealMatrix](new Array2DRowRealMatrix(Array[Double](0, 1)), new Array2DRowRealMatrix(Array[Double](2, 3)), new Array2DRowRealMatrix(Array[Double](4, 5)))
    val flatten1 = MatrixUtils.combinedMatrices(m1)
    val data = flatten1.getData
    Assert.assertEquals(6, data.length)
    Assert.assertArrayEquals(Array[Double](0), data(0), 0.0)
    Assert.assertArrayEquals(Array[Double](1), data(1), 0.0)
    Assert.assertArrayEquals(Array[Double](2), data(2), 0.0)
    Assert.assertArrayEquals(Array[Double](3), data(3), 0.0)
    Assert.assertArrayEquals(Array[Double](4), data(4), 0.0)
    Assert.assertArrayEquals(Array[Double](5), data(5), 0.0)
  }

  @Test def testCombinedMatrices2D(): Unit = {
    val m1 = Array[RealMatrix](new Array2DRowRealMatrix(Array[Array[Double]](Array[Double](1, 2, 3), Array[Double](4, 5, 6))), new Array2DRowRealMatrix(Array[Array[Double]](Array[Double](7, 8, 9), Array[Double](10, 11, 12))), new Array2DRowRealMatrix(Array[Array[Double]](Array[Double](13, 14, 15), Array[Double](16, 17, 18))))
    val flatten1 = MatrixUtils.combinedMatrices(m1)
    Assert.assertEquals(3, flatten1.getColumnDimension)
    Assert.assertEquals(6, flatten1.getRowDimension)
  }

  @Test def testCombinedMatrices2D_Toeplitz(): Unit = {
    val e1 = new Array2DRowRealMatrix(Array[Double](1, 1.1))
    val e2 = new Array2DRowRealMatrix(Array[Double](2, 2.2))
    val e2T = e2.transpose
    val e3 = new Array2DRowRealMatrix(Array[Double](3, 3.3))
    val e3T = e3.transpose
    val m1 = Array[RealMatrix](e1, e2, e3)
    // {1.0,1.1}
    // {2.0,2.2}
    // {3.0,3.3}
    val toeplitz1 = MatrixUtils.toeplitz(m1, 3)
    Assert.assertEquals(3, toeplitz1.length)
    Assert.assertEquals(3, toeplitz1(0).length)
    Assert.assertEquals(3, toeplitz1(1).length)
    Assert.assertEquals(3, toeplitz1(2).length)
    Assert.assertEquals(e1, toeplitz1(0)(0))
    Assert.assertEquals(e1, toeplitz1(1)(1))
    Assert.assertEquals(e1, toeplitz1(2)(2))
    Assert.assertEquals(e2, toeplitz1(1)(0))
    Assert.assertEquals(e2, toeplitz1(2)(1))
    Assert.assertEquals(e3, toeplitz1(2)(0))
    Assert.assertEquals(e2T, toeplitz1(0)(1))
    Assert.assertEquals(e2T, toeplitz1(1)(2))
    Assert.assertEquals(e3T, toeplitz1(0)(2))
    // {1.0,1.1}  {2.0,2.2}' {3.0,3.3}'
    // {2.0,2.2}  {1.0,1.1}  {2.0,2.2}'
    // {3.0,3.3}  {2.0,2.2}  {1.0,1.1}
    val flatten1 = MatrixUtils.combinedMatrices(toeplitz1, 2)
    // 1.0 0.0 2.0 2.2 3.0 3.3
    // 1.1 0.0 0.0 0.0 0.0 0.0
    // 2.0 0.0 1.0 0.0 2.0 2.2
    // 2.2 0.0 1.1 0.0 0.0 0.0
    // 3.0 0.0 2.0 0.0 1.0 0.0
    // 3.3 0.0 2.2 0.0 1.1 0.0
    Assert.assertEquals(6, flatten1.getRowDimension)
    Assert.assertEquals(6, flatten1.getColumnDimension)
  }

  //  @Test def testFlatten1D(): Unit = {
  //    val m1 = Array[RealMatrix](new Array2DRowRealMatrix(Array[Double](0, 1)), new Array2DRowRealMatrix(Array[Double](2, 3)), new Array2DRowRealMatrix(Array[Double](4, 5)))
  //    val actual = MatrixUtils.flatten(m1)
  //    Assert.assertArrayEquals(actual, Array[Double](0, 1, 2, 3, 4, 5), 0.d)
  //  }
  //
  //  @Test def testFlatten2D(): Unit = {
  //    val m1 = Array[RealMatrix](new Array2DRowRealMatrix(Array[Array[Double]](Array[Double](1, 2, 3), Array[Double](4, 5, 6))), new Array2DRowRealMatrix(Array[Array[Double]](Array[Double](7, 8, 9), Array[Double](10, 11, 12))), new Array2DRowRealMatrix(Array[Array[Double]](Array[Double](13, 14, 15), Array[Double](16, 17, 18))))
  //    val actual = MatrixUtils.flatten(m1)
  //    val expected = new Array[Double](18)
  //    for ( i  <- 0 until expected.length)
  //    {
  //      expected(i) = i + 1
  //    }
  //    Assert.assertArrayEquals(expected, actual, 0.d)
  //  }

  @Test def testUnflatten2D(): Unit = {
    val data = new Array[Double](24)

    for (i <- 0 until data.length) {
      data(i) = i + 1
    }
    val actual = MatrixUtils.unflatten(data, 2, 3, 4)
    val expected = Array[RealMatrix](new Array2DRowRealMatrix(Array[Array[Double]](Array[Double](1, 2, 3), Array[Double](4, 5, 6))), new Array2DRowRealMatrix(Array[Array[Double]](Array[Double](7, 8, 9), Array[Double](10, 11, 12))), new Array2DRowRealMatrix(Array[Array[Double]](Array[Double](13, 14, 15), Array[Double](16, 17, 18))), new Array2DRowRealMatrix(Array[Array[Double]](Array[Double](19, 20, 21), Array[Double](22, 23, 24))))
    //    Assert.assertArrayEquals(expected, actual)
  }

  @Test def testPower1(): Unit = {
    val A = new Array2DRowRealMatrix(Array[Array[Double]](Array[Double](1, 2, 3), Array[Double](4, 5, 6)))
    val x = new Array[Double](3)
    x(0) = Math.random
    x(1) = Math.random
    x(2) = Math.random
    val u = new Array[Double](2)
    val v = new Array[Double](3)
    val s: Double = MatrixUtils.power1(A, x, 2, u, v)
    val svdA = new SingularValueDecomposition(A)
    Assert.assertArrayEquals(svdA.getU.getColumn(0), u, 0.001)
    Assert.assertArrayEquals(svdA.getV.getColumn(0), v, 0.001)
    Assert.assertEquals(svdA.getSingularValues()(0), s, 0.001)
  }

  @Test def testLanczosTridiagonalization(): Unit = { // Symmetric matrix
    val C = new Array2DRowRealMatrix(Array[Array[Double]](Array[Double](1, 2, 3, 4), Array[Double](2, 1, 4, 3), Array[Double](3, 4, 1, 2), Array[Double](4, 3, 2, 1)))
    // naive initial vector
    val a = Array[Double](1, 1, 1, 1)
    val actual = new Array2DRowRealMatrix(Array.ofDim[Double](4, 4))
    MatrixUtils.lanczosTridiagonalization(C, a, actual)
    val expected = new Array2DRowRealMatrix(Array[Array[Double]](Array[Double](40, 60, 0, 0), Array[Double](60, 10, 120, 0), Array[Double](0, 120, 10, 120), Array[Double](0, 0, 120, 10)))
    Assert.assertEquals(expected, actual)
  }

  @Test def testTridiagonalEigen(): Unit = { // Tridiagonal Matrix
    val T = new Array2DRowRealMatrix(Array[Array[Double]](Array[Double](40, 60, 0, 0), Array[Double](60, 10, 120, 0), Array[Double](0, 120, 10, 120), Array[Double](0, 0, 120, 10)))
    val eigvals = new Array[Double](4)
    val eigvecs = new Array2DRowRealMatrix(Array.ofDim[Double](4, 4))
    MatrixUtils.tridiagonalEigen(T, 2, eigvals, eigvecs)
    val actual = eigvecs.multiply(eigvecs.transpose)
    val expected = new Array2DRowRealMatrix(Array.ofDim[Double](4, 4))
    for (i <- 0 until 4) {
      expected.setEntry(i, i, 1)
    }
    for (i <- 0 until 4) {
      for (j <- 0 until 4) {
        Assert.assertEquals(expected.getEntry(i, j), actual.getEntry(i, j), 0.001d)
      }

    }
  }

  // scalastyle:on
}
