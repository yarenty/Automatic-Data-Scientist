package com.yarenty.math

import org.apache.commons.math3.linear._

// scalastyle:off
object MatrixUtils {
  /**
    * Solve Yule-walker equation by Levinson-Durbin Recursion.
    *
    * <pre>
    * R_j = ∑_{i=1} ^^ {k} A_i R_{j-i} where j = 1..k, R_{-i} = R'_i
    * </pre>
    *
    * @see http://www.emptyloop.com/technotes/a%20tutorial%20on%20linear%20prediction%20and%20levinson-durbin.pdf
    *      Cedrick Collomb: A tutorial on linear prediction and Levinson-Durbin</a>
    * @param R autocovariance where |R| >= order
    * @param A coefficient to be solved where |A| >= order + 1
    * @return E variance of prediction error
    */
  def aryule(R: Array[Double], A: Array[Double], order: Int): Array[Double] = {
    assert(R.length > order, "|R| MUST be greater than or equals to " + order + ": " + R.length)
    assert(A.length >= order + 1, "|A| MUST be greater than or equals to " + (order + 1) + ": " + A.length)

    val E = new Array[Double](order + 1)
    A(0) = 1.0d
    E(0) = R(0)

    for (k <- 0 until order) {
      var lambda = 0.0

      for (j <- 0 to k) {
        lambda -= A(j) * R(k + 1 - j)
      }

      val Ek = E(k)
      if (Ek == 0.0) lambda = 0.0
      else lambda /= Ek

      val last = (k + 1) / 2

      for (n <- 0 to last) {
        val i = k + 1 - n
        val tmp = A(i) + lambda * A(n)
        A(n) += lambda * A(i)
        A(i) = tmp

      }
      E(k + 1) = Ek * (1.0d - lambda * lambda)
    }

    for (i <- 0 to order) {
      A(i) = -A(i)
    }
    E
  }


  /**
    * Fit an AR(order) model using the Burg's method.
    *
    * @see https://searchcode.com/codesearch/view/9503568/
    * @param X data vector to estimate where |X| >= order
    * @param A coefficient to be solved where |A| >= order + 1
    * @return E variance of white noise
    */
  def arburg(X: Array[Double], A: Array[Double], order: Int): Array[Double] = {
    assert(X.length > order, "|X| MUST be greater than or equals to " + order + ": " + X.length)
    assert(A.length >= order + 1, "|A| MUST be greater than or equals to " + (order + 1) + ": " + A.length)
    val nDataPoints = X.length
    val E = new Array[Double](order + 1)
    E(0) = 0.0d

    for (i <- 0 until nDataPoints) {
      E(0) += X(i) * X(i)
    }
    // f and b are the forward and backward error sequences
    var currentErrorSequenceSize = nDataPoints - 1
    val F = new Array[Double](currentErrorSequenceSize)
    val B = new Array[Double](currentErrorSequenceSize)

    for (i <- 0 until currentErrorSequenceSize) {
      F(i) = X(i + 1)
      B(i) = X(i)
    }
    A(0) = 1.0d
    // remaining stages i=2 to p
    for (i <- 0 until order) { // get the i-th reflection coefficient
      var numerator = 0.0d
      var denominator = 0.0d

      for (j <- 0 until currentErrorSequenceSize) {
        numerator += F(j) * B(j)
        denominator += F(j) * F(j) + B(j) * B(j)

      }
      numerator *= 2.0d
      var g = 0.0d
      if (denominator != 0.0d) g = numerator / denominator
      // generate next filter order
      val prevA = Array.ofDim[Double](i)

      for (j <- 0 until i) { // No need to copy A[0] = 1.0
        prevA(j) = A(j + 1)
      }
      A(1) = g
      for (j <- 1 until i + 1) {
        A(j + 1) = prevA(j - 1) - g * prevA(i - j)
      }

      // keep track of the error
      E(i + 1) = E(i) * (1 - g * g)
      // update the prediction error sequences
      val prevF = new Array[Double](currentErrorSequenceSize)

      for (j <- 0 until currentErrorSequenceSize) {
        prevF(j) = F(j)

      }
      val nextErrorSequenceSize = nDataPoints - i - 2

      for (j <- 0 until nextErrorSequenceSize) {
        F(j) = prevF(j + 1) - g * B(j + 1)
        B(j) = B(j) - g * prevF(j)

      }
      currentErrorSequenceSize = nextErrorSequenceSize

    }
    val mid = order / 2 + 1

    for (i <- 1 until mid) { // Reverse 1..(order - 1)-th elements by swapping
      val tmp = A(i)
      A(i) = A(order + 1 - i)
      A(order + 1 - i) = tmp

    }
    for (i <- 0 to order) {
      A(i) = -A(i)

    }
    E
  }

  /**
    * Construct a Toeplitz matrix.
    */
  def toeplitz(c: Array[RealMatrix], dim: Int): Array[Array[RealMatrix]] = {
    assert(dim >= 1, "Invalid dimension: " + dim)
    assert(c.length >= dim, "|c| must be greather than " + dim + ": " + c.length)
    /*
             * Toeplitz matrix  (symmetric, invertible, k*dimensions by k*dimensions)
             *
             * /C_0     |C_1'    |C_2'     | .  .  .  |C_{k-1}' \
             * |--------+--------+--------+           +---------|
             * |C_1     |C_0     |C_1'     |               .    |
             * |--------+--------+--------+                .    |
             * |C_2     |C_1     |C_0      |               .    |
             * |--------+--------+--------+                     |
             * |   .                         .                  |
             * |   .                            .               |
             * |   .                               .            |
             * |--------+                              +--------|
             * \C_{k-1} | .  .  .                      |C_0     /
             */ val c0 = c(0)
    val toeplitz = Array.ofDim[RealMatrix](dim, dim)

    for (row <- 0 until dim) {
      toeplitz(row)(row) = c0
      for (col <- 0 until dim) {
        if (row < col) toeplitz(row)(col) = c(col - row).transpose
        else if (row > col) toeplitz(row)(col) = c(row - col)
      }
    }
    toeplitz
  }

  def toeplitz(c: Array[Double]): Array[Array[Double]] = toeplitz(c, c.length)

  def toeplitz(c: Array[Double], dim: Int): Array[Array[Double]] = {
    assert(dim >= 1, "Invalid dimension: " + dim)
    assert(c.length >= dim, "|c| must be greather than " + dim + ": " + c.length)
    val c0 = c(0)
    val toeplitz = Array.ofDim[Double](dim, dim)
    for (row <- 0 until dim) {
      toeplitz(row)(row) = c0
      for (col <- 0 until dim) {
        if (row < col) toeplitz(row)(col) = c(col - row)
        else if (row > col) toeplitz(row)(col) = c(row - col)
      }
    }
    toeplitz
  }

  //   def flatten( grid: Array[Array[RealMatrix]]): Array[Double] = {
  //    //    assert(grid.length >= 1, "The number of rows must be greather than 1")
  //    //    assert(grid(0).length >= 1, "The number of cols must be greather than 1")
  //    //    val rows = grid.length
  //    //    val cols = grid(0).length
  //    //    val grid00 = grid(0)(0)
  //    //    assert(null != grid00)
  //    //    val cellRows = grid00.getRowDimension
  //    //    val cellCols = grid00.getColumnDimension
  //    //    val list = List(rows * cols * cellRows * cellCols)
  //    grid.flatten
  //    //    val visitor = new DefaultRealMatrixPreservingVisitor() {
  //    //      override def visit(row: Int, column: Int, value: Double): Unit = {
  //    //        list.add(value)
  //    //      }
  //    //    }
  //    //    for (row <- 0 until dim)  {
  //    //      for (col <- 0 until dim)   {
  //    //        val cell = grid(row)(col)
  //    //        cell.walkInRowOrder(visitor)
  //    //      }
  //    //    }
  //    //    list.toArray
  //  }
  //
  //   def flatten( grid: Array[RealMatrix]): Array[Double] = {
  //    grid.flatten
  //
  //    //    assert(grid.length >= 1, "The number of rows must be greather than 1")
  //    //    val rows = grid.length
  //    //    val grid0 = grid(0)
  //    //    assert(null != grid0)
  //    //    val cellRows = grid0.getRowDimension
  //    //    val cellCols = grid0.getColumnDimension
  //    //    val list = new DoubleArrayList(rows * cellRows * cellCols)
  //    //    val visitor = new DefaultRealMatrixPreservingVisitor() {
  //    //      override def visit(row: Int, column: Int, value: Double): Unit = {
  //    //        list.add(value)
  //    //      }
  //    //    }
  //    //    for (row <- 0 until rows )
  //    //    {
  //    //      val cell = grid(row)
  //    //      cell.walkInRowOrder(visitor)
  //    //    }
  //    //    list.toArray
  //  }

  def unflatten(data: Array[Double], rows: Int, cols: Int, len: Int): Array[RealMatrix] = {
    val grid = new Array[RealMatrix](len)
    var offset = 0

    for (k <- 0 until len) {
      val cell = new BlockRealMatrix(rows, cols)
      grid(k) = cell
      for (i <- 0 until rows) {
        for (j <- 0 until cols) {
          if (offset >= data.length) throw new IndexOutOfBoundsException("Offset " + offset + " exceeded data.length " + data.length)
          val value = data(offset)
          cell.setEntry(i, j, value)
          offset += 1
        }
      }
    }
    if (offset != data.length) throw new IllegalArgumentException("Invalid data for unflatten")
    grid
  }

  def combinedMatrices(grid: Array[Array[RealMatrix]], dimensions: Int): RealMatrix = {
    assert(grid.length >= 1, "The number of rows must be greather than 1")
    assert(grid(0).length >= 1, "The number of cols must be greather than 1")
    assert(dimensions > 0, "Dimension should be more than 0: " + dimensions)
    val rows = grid.length
    val cols = grid(0).length
    val combined = new BlockRealMatrix(rows * dimensions, cols * dimensions)
    for (row <- grid.indices) {
      for (col <- grid(row).indices) {
        combined.setSubMatrix(grid(row)(col).getData, row * dimensions, col * dimensions)
      }
    }
    combined
  }

  def combinedMatrices(grid: Array[RealMatrix]): RealMatrix = {
    assert(grid.length >= 1, "The number of rows must be greather than 0: " + grid.length)
    val rows = grid.length
    val rowDims = grid(0).getRowDimension
    val colDims = grid(0).getColumnDimension
    val combined = new BlockRealMatrix(rows * rowDims, colDims)
    for (row <- grid.indices) {
      val cell = grid(row)
      assert(cell.getRowDimension == rowDims, "Mismatch in row dimensions at row " + row)
      assert(cell.getColumnDimension == colDims, "Mismatch in col dimensions at row " + row)
      combined.setSubMatrix(cell.getData, row * rowDims, 0)

    }
    combined
  }


  @throws[SingularMatrixException]
  def inverse(m: RealMatrix): RealMatrix = inverse(m, true)


  @throws[SingularMatrixException]
  def inverse(m: RealMatrix, exact: Boolean): RealMatrix = {
    val LU = new LUDecomposition(m)
    val solver = LU.getSolver

    if (exact || solver.isNonSingular) solver.getInverse
    else {
      val SVD = new SingularValueDecomposition(m)
      SVD.getSolver.getInverse
    }

  }

  def det(m: RealMatrix): Double = {
    val LU = new LUDecomposition(m)
    LU.getDeterminant
  }

  /**
    * Return a 2-D array with ones on the diagonal and zeros elsewhere.
    */
  def eye(n: Int): Array[Array[Double]] = {
    val eye = Array.ofDim[Double](n, n)
    for (i <- 0 until n) {
      eye(i)(i) = 1
    }
    eye
  }

  /**
    * L = A x R
    *
    * @return a matrix A that minimizes A x R - L
    */

  @throws[SingularMatrixException]
  def solve(L: RealMatrix, R: RealMatrix): RealMatrix = solve(L, R, true)


  @throws[SingularMatrixException]
  def solve(L: RealMatrix, R: RealMatrix, exact: Boolean): RealMatrix = {
    val LU = new LUDecomposition(R)
    val solver = LU.getSolver

    if (exact || solver.isNonSingular) LU.getSolver.solve(L)
    else {
      val SVD = new SingularValueDecomposition(R)
      SVD.getSolver.solve(L)
    }

  }

  /**
    * Find the first singular vector/value of a matrix A based on the Power method.
    *
    * @see http://www.cs.yale.edu/homes/el327/datamining2013aFiles/07_singular_value_decomposition.pdf
    * @param A     target matrix
    * @param x0    initial vector
    * @param nIter number of iterations for the Power method
    * @param u     1st left singular vector
    * @param v     1st right singular vector
    * @return 1st singular value
    */
  def power1(A: RealMatrix, x0: Array[Double], nIter: Int, u: Array[Double], v: Array[Double]): Double = {
    assert(A.getColumnDimension == x0.length, "Column size of A and length of x should be same")
    assert(A.getRowDimension == u.length, "Row size of A and length of u should be same")
    assert(x0.length == v.length, "Length of x and u should be same")
    assert(nIter >= 1, "Invalid number of iterations: " + nIter)
    val AtA = A.transpose.multiply(A)
    var x: RealVector = new ArrayRealVector(x0)
    for (i <- 0 until nIter) {
      x = AtA.operate(x)
    }
    val xNorm = x.getNorm

    for (i <- v.indices) {
      v(i) = x.getEntry(i) / xNorm

    }
    val Av = new ArrayRealVector(A.operate(v))
    val s = Av.getNorm

    for (i <- u.indices) {
      u(i) = Av.getEntry(i) / s


    }
    s
  }

  /**
    * Lanczos tridiagonalization for a symmetric matrix C to make s * s tridiagonal matrix T.
    *
    * @see http://www.cas.mcmaster.ca/~qiao/publications/spie05.pdf
    * @param C target symmetric matrix
    * @param a initial vector
    * @param T result is stored here
    */
  def lanczosTridiagonalization(C: RealMatrix, a: Array[Double], T: RealMatrix): Unit = {
    //    assert(java.util.Arrays.deepEquals(C.getData, C.transpose.getData), "Target matrix C must be a symmetric matrix")
    assert(C.getColumnDimension == a.length, "Column size of A and length of a should be same")
    assert(T.getRowDimension == T.getColumnDimension, "T must be a square matrix")
    val s = T.getRowDimension
    // initialize T with zeros
    T.setSubMatrix(Array.ofDim[Double](s, s), 0, 0)
    var a0: RealVector = new ArrayRealVector(a.length)
    var r: RealVector = new ArrayRealVector(a)
    var beta0 = 1.0
    for (i <- 0 until s) {
      val a1 = r.mapDivide(beta0)
      val Ca1 = C.operate(a1)
      val alpha1 = a1.dotProduct(Ca1)
      r = Ca1.add(a1.mapMultiply(-1.0 * alpha1)).add(a0.mapMultiply(-1.0 * beta0))
      val beta1 = r.getNorm
      T.setEntry(i, i, alpha1)
      if (i - 1 >= 0) T.setEntry(i, i - 1, beta0)
      if (i + 1 < s) T.setEntry(i, i + 1, beta1)
      a0 = a1.copy
      beta0 = beta1

    }
  }

  /**
    * QR decomposition for a tridiagonal matrix T.
    *
    * @see https://gist.github.com/lightcatcher/8118181
    * @see http://www.ericmart.in/blog/optimizing_julia_tridiag_qr
    * @param T  target tridiagonal matrix
    * @param R  output matrix for R which is the same shape as T
    * @param Qt output matrix for Q.T which is the same shape an T
    */
  def tridiagonalQR(T: RealMatrix, R: RealMatrix, Qt: RealMatrix): Unit = {
    val n = T.getRowDimension
    assert(n == R.getRowDimension && n == R.getColumnDimension, "T and R must be the same shape")
    assert(n == Qt.getRowDimension && n == Qt.getColumnDimension, "T and Qt must be the same shape")
    // initial R = T
    R.setSubMatrix(T.getData, 0, 0)
    // initial Qt = identity
    Qt.setSubMatrix(eye(n), 0, 0)

    for (i <- 0 until n - 1) { // Householder projection for a vector x
      // https://en.wikipedia.org/wiki/Householder_transformation
      var x = T.getSubMatrix(i, i + 1, i, i).getColumnVector(0)
      x = unitL2norm(x)
      val subR = R.getSubMatrix(i, i + 1, 0, n - 1)
      R.setSubMatrix(subR.subtract(x.outerProduct(subR.preMultiply(x)).scalarMultiply(2)).getData, i, 0)
      val subQt = Qt.getSubMatrix(i, i + 1, 0, n - 1)
      Qt.setSubMatrix(subQt.subtract(x.outerProduct(subQt.preMultiply(x)).scalarMultiply(2)).getData, i, 0)

    }
  }

  def sign(x: Double): Double = {
    if (x < 0.0) -1.0 else if (x > 0.0) 1.0 else 0
  }

  def unitL2norm(x: RealVector): RealVector = {
    val x0 = x.getEntry(0)
    val s = sign(x0)
    x.setEntry(0, x0 + s * x.getNorm)
    x.unitVector
  }

  /**
    * Find eigenvalues and eigenvectors of given tridiagonal matrix T.
    *
    * @see http://web.csulb.edu/~tgao/math423/s94.pdf
    * @see http://stats.stackexchange.com/questions/20643/finding-matrix-eigenvectors-using-qr-decomposition
    * @param T       target tridiagonal matrix
    * @param nIter   number of iterations for the QR method
    * @param eigvals eigenvalues are stored here
    * @param eigvecs eigenvectors are stored here
    */
  def tridiagonalEigen(T: RealMatrix, nIter: Int, eigvals: Array[Double], eigvecs: RealMatrix): Unit = {
    //    assert(java.util.Arrays.deepEquals(T.getData, T.transpose.getData), "Target matrix T must be a symmetric (tridiagonal) matrix")
    assert(eigvecs.getRowDimension == eigvecs.getColumnDimension, "eigvecs must be a square matrix")
    assert(T.getRowDimension == eigvecs.getRowDimension, "T and eigvecs must be the same shape")
    assert(eigvals.length == eigvecs.getRowDimension, "Number of eigenvalues and eigenvectors must be same")
    val nEig = eigvals.length
    // initialize eigvecs as an identity matrix
    eigvecs.setSubMatrix(eye(nEig), 0, 0)
    var t_ : RealMatrix = T.copy
    for (i <- 0 until nIter) { // QR decomposition for the tridiagonal matrix T
      val R = new Array2DRowRealMatrix(nEig, nEig)
      val Qt = new Array2DRowRealMatrix(nEig, nEig)
      tridiagonalQR(t_, R, Qt)
      val Q = Qt.transpose
      t_ = R.multiply(Q)
      eigvecs.setSubMatrix(eigvecs.multiply(Q).getData, 0, 0)
    }
    // diagonal elements correspond to the eigenvalues
    for (i <- 0 until nEig) {
      eigvals(i) = t_.getEntry(i, i)
    }
  }
}

// scalastyle:on
