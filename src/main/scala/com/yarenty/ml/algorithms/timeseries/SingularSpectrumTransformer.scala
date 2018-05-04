package com.yarenty.ml.algorithms.timeseries

import com.yarenty.ml.algorithms.timeseries.SingularSpectrumTransformer.{IKA, SVD, ScoreFunction}
import com.yarenty.ml.algorithms.utils.MatrixUtils
import javax.annotation.Nonnull
import org.apache.commons.math3.linear.{Array2DRowRealMatrix, RealMatrix, SingularValueDecomposition}

import scala.collection.immutable.TreeMap
import scala.collection.mutable.ArrayBuffer


/**
  * Change-point detection based on Singular Spectrum Transformation (SST).
  *
  * References:
  * T. Ide and K. Inoue, "Knowledge Discovery from Heterogeneous Dynamic Systems using Change-Point Correlations", SDM'05.
  * T. Ide and K. Tsuda, "Change-point detection using Krylov subspace learning", SDM'07.
  */
object SingularSpectrumTransformer {

  trait ScoreFunction

  // scalastyle:off
  class Parameters(
                    var window: Int = 24,
                    var n_past: Int = 24,
                    var n_current: Int = 24,
                    var current_offset: Int = -24,
                    var n_component: Int = 3,
                    var n_dim: Int = 5,
                    @Nonnull var scoreFunc: ScoreFunction = SVD,
                    var changepointThreshold: Double = -1.0
                  ) {

    def set(@Nonnull func: ScoreFunction): Unit = {
      this.scoreFunc = func
    }

    def setWindow(w: Int): Unit = {
      this.window = w
      this.n_past = w
      this.n_current = w
      this.current_offset = -w
    }

    def setComponent(r: Int): Unit = {
      this.n_component = r
      this.n_dim = if (r % 2 == 0) 2 * r else 2 * r - 1
    }

    override def toString: String =
      s"""
         | window = $window - Number of samples which affects change-point score [default: 24]
         | n_past = $n_past - Number of past windows for change-point scoring [default: equal to `window` = 24]
         | n_current = $n_current - Number of current windows for change-point scoring [default: equal to `window` = 24]
         | current_offset = $current_offset - Offset of the current windows from the updating sample [default: `-window` = -24]
         | n_component = $n_component - Number of singular vectors (i.e. principal components) [default: 3]
         | n_dim = $n_dim - Number of dimensions for the Krylov subspaces [default: 5 (`2*r` if `r` is even, `2*r-1` otherwise)] r=n_comoponent
         | scorefunc = $scoreFunc - Score function [default: svd, ika]
         | changepointThreshold = $changepointThreshold - Score threshold (inclusive) for determining change-point existence
         | [default: -1, do not output decision]
      """.stripMargin
  }

  case object SVD extends ScoreFunction //Singular Value Decomposition (SVD)

  case object IKA extends ScoreFunction //Implicit Krylov Approximation (IKA)

  // scalastyle:on
}


class SingularSpectrumTransformer(@Nonnull val params: SingularSpectrumTransformer.Parameters) {

  assert(params.window >= 2, "window must be greather than 1: " + params.window)
  assert(params.n_component >= 1, "r must be greater than 0: " + params.n_component)
  assert(params.n_dim >= 1, "k must be greater than 0: " + params.n_dim)
  assert(params.n_dim >= params.n_component, "k must be equals to or greather than r: k=" + params.n_dim + ", r=" + params.n_component)
  // assert(changepointThreshold > 0.0d && changepointThreshold < 1.0d,"changepointThreshold must be in range (0, 1): " + changepointThreshold)


  // (w + n) past samples for the n-past-windows
  // (w + m) current samples for the m-current-windows, starting from offset g
  // => need to hold past (w + n + g + w + m) samples from the latest sample
  var nCurrentWindow: Int = params.n_current
  var window: Int = params.window
  var nPastWindow: Int = params.n_past
  var pastSize: Int = window + nPastWindow
  var currentSize: Int = window + nCurrentWindow
  var currentOffset: Int = params.current_offset
  val holdSampleSize: Int = pastSize + currentOffset + currentSize
  var scoreFunc: ScoreFunction = params.scoreFunc
  var r = params.n_component


  assert(params.n_dim >= params.n_component)
  var k = params.n_dim
  var xRing = new ArrayBuffer[Double](holdSampleSize) //RingBuffer
  var xSeries = new Array[Double](holdSampleSize)
  var q = new Array[Double](window)
  var norm: Double = 0.0

  for (i <- 0 until window) {
    q(i) = Math.random
    norm += q(i) * q(i)

  }
  norm = Math.sqrt(norm)
  // normalize
  for (i <- 0 until window) {
    q(i) = q(i) / norm
  }


  def update(arg: Double, outScores: Array[Double]): Unit = {
    val x = arg
    xRing += x
    xSeries = xRing.toArray

    // need to wait until the buffer is filled
    if (xRing.length < holdSampleSize) {
      outScores(0) = 0.0
    }
    else {
      // create past trajectory matrix and find its left singular vectors
      val H = new Array2DRowRealMatrix(window, nPastWindow)

      for (i <- 0 until nPastWindow) {
        H.setColumn(i, java.util.Arrays.copyOfRange(xSeries, i, i + window))
      }

      // create current trajectory matrix and find its left singular vectors
      val G = new Array2DRowRealMatrix(window, nCurrentWindow)
      val currentHead = pastSize + currentOffset
      for (i <- 0 until nCurrentWindow) {
        G.setColumn(i, java.util.Arrays.copyOfRange(xSeries, currentHead + i, currentHead + i + window))
      }

      scoreFunc match {
        case SVD =>
          outScores(0) = computeScoreSVD(H, G)
        case IKA =>
          outScores(0) = computeScoreIKA(H, G)
        case _ =>
          throw new IllegalStateException("Unexpected score function: " + scoreFunc)
      }
      xRing.remove(0) // like ring functionality
    }
  }

  /**
    * Singular Value Decomposition (SVD) based naive scoring.
    */
  private def computeScoreSVD(@Nonnull H: RealMatrix, @Nonnull G: RealMatrix): Double = {
    val svdH = new SingularValueDecomposition(H)
    val UT = svdH.getUT
    val svdG = new SingularValueDecomposition(G)
    val Q = svdG.getU

    // find the largest singular value for the r principal components
    val UTQ = UT.getSubMatrix(0, r - 1, 0, window - 1).multiply(Q.getSubMatrix(0, window - 1, 0, r - 1))
    val svdUTQ = new SingularValueDecomposition(UTQ)
    val s = svdUTQ.getSingularValues
    1.0 - s(0)
  }

  /**
    * Implicit Krylov Approximation (IKA) based naive scoring.
    *
    * Number of iterations for the Power method and QR method is fixed to 1 for efficiency. This
    * may cause failure (i.e. meaningless scores) depending on datasets and initial values.
    *
    */
  private def computeScoreIKA(@Nonnull H: RealMatrix, @Nonnull G: RealMatrix): Double = {

    // assuming n = m = window, and keep track the left singular vector as `q`
    MatrixUtils.power1(G, q, 1, q, new Array[Double](window))
    val T = new Array2DRowRealMatrix(k, k)
    MatrixUtils.lanczosTridiagonalization(H.multiply(H.transpose), q, T)
    val eigvals = Array.ofDim[Double](k)
    val eigvecs = new Array2DRowRealMatrix(k, k)
    MatrixUtils.tridiagonalEigen(T, 1, eigvals, eigvecs)

    // tridiagonalEigen() returns unordered eigenvalues,
    // so the top-r eigenvectors should be picked carefully
    var map: TreeMap[Double, Integer] = new TreeMap[Double, Integer]()(Ordering.Double.reverse)

    for (i <- 0 until k) {
      map = map + (eigvals(i) -> i)
    }

    val indicies = map.values.iterator
    var s = 0.0
    for (i <- 0 until r) {
      if (!indicies.hasNext) throw new IllegalStateException("Should not happen")
      val v = eigvecs.getEntry(0, indicies.next.intValue)
      s += v * v
    }
    1.0 - Math.sqrt(s)
  }
}
