package com.yarenty.ml.algorithms.timeseries

import java.util

import com.yarenty.ml.algorithms.LinearRegression

import scala.collection.mutable.ListBuffer

/**
  * (C)2018 by yarenty
  */
class ResultsLinReg(val changes: List[Int], val residuals: List[Double], val predictions: List[Double]) {}


object ChangePointDetection {

  val adFreq = 24

  def detect(x: Array[Double], expectedChange: Double = 0.05, threshold: Double = .02): (Boolean, List[Int]) = {
    val det = CusumLinReg(x, adFreq, expectedChange, threshold)
    val d0: Int = if (det.changes.nonEmpty) det.changes.head else x.length - 1
    val det2 = CusumLvl(x, adFreq, expectedChange, threshold)
    val d1: Int = if (det2.nonEmpty) det2.head else x.length - 1
    val out: List[Int] = (det.changes ++ det2).distinct.sorted
    ((Math.max(0, Math.min(d0, d1)) != 0) && out.nonEmpty, out)
  }


  def CusumLinReg(signal: Array[Double], update_width: Int, expected_change: Double, threshold: Double): ResultsLinReg = {

    val chps = ListBuffer[Int]()
    val update_points = ListBuffer[Int]()
    var mu0, mu1, mu2, k_up, k_down = 0.0
    val n = signal.length
    var wait_flag = 0
    // Linear Regression part
    val signal_resid: Array[Double] = Array.ofDim[Double](n)
    val predictions: Array[Double] = Array.ofDim[Double](n)
    var K = 1.0
    var B = 0.0
    // End Linear Regression part
    val stat_up: Array[Double] = Array.ofDim[Double](n)
    val stat_down: Array[Double] = Array.ofDim[Double](n)
    for (i <- 0 until n) {
      if (i == update_width) {
        // Initialization
        val subdat = util.Arrays.copyOfRange(signal, i - update_width, i)
        // Linear Regression part
        val lsq = new LinearRegression(subdat)
        K = lsq.coeff_k
        B = lsq.coeff_b
        val residuals = LinearRegression.error(subdat, K, B)
        var idx = 0
        for (ii <- (i - update_width) until i) {
          predictions(ii) = K * (idx + 1) + B
          signal_resid(ii) = residuals(idx)
          idx += 1
        }
        mu0 = residuals.sum / residuals.length.toDouble
        // mu0 = Utils.mean(subdat); - level shift case
        // End Linear Regression part
        mu1 = mu0 + expected_change
        mu2 = mu0 - expected_change
        k_up = (mu1 + mu0) / 2
        k_down = (mu2 + mu0) / 2 // ?
        update_points += i
      }

      if (i > update_width) {
        predictions(i) = K * (i - update_points.last) + B
        signal_resid(i) = signal(i) - predictions(i)

        if (wait_flag == 0) {
          // Linear Regression part
          //predictions[i] = K * ( i - ( update_points.get(update_points.size() - 1) ) + 0) + B ;
          // stat_up[i]    = Math.max( 0, stat_up[i-1]   + signal[i] - k_up   );
          stat_up(i) = Math.max(0, stat_up(i - 1) + signal_resid(i) - k_up)
          // End Linear Regression part
          // stat_down[i]  = Math.min( 0, stat_down[i-1] + signal[i] - k_down );
          stat_down(i) = Math.min(0, stat_down(i - 1) + signal_resid(i) - k_down)
        } else {
          stat_up(i) = 0
          stat_down(i) = 0
        }

        // Change detection
        if (stat_up(i) > threshold || Math.abs(stat_down(i)) > threshold) {
          chps += i
          wait_flag = 1
        }

        // Keep wait flag 1 until update_width is reached
        if (chps.nonEmpty && wait_flag == 1) {
          stat_up(i) = 0
          stat_down(i) = 0
          if (i - chps.last <= update_width)
            wait_flag = 1
          else
            wait_flag = 0
        }

        // Update change detector parameters
        if (chps.nonEmpty) {
          if (i - chps.last == update_width) {
            val subdat = util.Arrays.copyOfRange(signal, i - update_width, i)
            // Linear Regression part
            val lsq = new LinearRegression(subdat)
            K = lsq.coeff_k
            B = lsq.coeff_b
            val residuals = LinearRegression.error(subdat, K, B)
            var idx = 0
            for (ii <- (i - update_width) until i) {
              predictions(ii) = K * (idx + 1) + B
              signal_resid(ii) = residuals(idx)
              idx += 1
            }
            mu0 = residuals.sum / residuals.length.toDouble
            // mu0 = Utils.mean(subdat); - level shift case
            // End Linear Regression part
            mu0 = subdat.sum / subdat.length.toDouble
            mu1 = mu0 + expected_change
            mu2 = mu0 - expected_change
            k_up = (mu1 + mu0) / 2
            k_down = (mu2 + mu0) / 2
            update_points += i
          }
        }
      }
    }
    new ResultsLinReg(chps.toList, predictions.toList, signal_resid.toList)

  }

  def CusumLvl(signal: Array[Double], update_width: Int, expected_change: Double, threshold: Double): List[Int] = {
    val chps = ListBuffer[Int]()
    val update_points = ListBuffer[Int]()
    var mu0, mu1, mu2, k_up, k_down = 0.0
    val n = signal.length

    var wait_flag = 0

    val stat_up = Array.ofDim[Double](n)
    val stat_down = Array.ofDim[Double](n)
    for (i <- 0 until n) {
      if (i == update_width) {
        // Initialization
        val subdat = util.Arrays.copyOfRange(signal, i - update_width, i)
        mu0 = subdat.sum / subdat.length.toDouble
        mu1 = mu0 + expected_change
        mu2 = mu0 - expected_change
        k_up = (mu1 + mu0) / 2
        k_down = (mu2 + mu0) / 2 // ?
        update_points += i
      }

      if (i > update_width) {
        if (wait_flag == 0) {
          stat_up(i) = Math.max(0, stat_up(i - 1) + signal(i) - k_up)
          stat_down(i) = Math.min(0, stat_down(i - 1) + signal(i) - k_down)
        } else {
          stat_up(i) = 0
          stat_down(i) = 0
        }

        // Change detection
        if (stat_up(i) > threshold || Math.abs(stat_down(i)) > threshold) {
          chps += i
          wait_flag = 1
        }

        // Keep wait flag 1 until update_width is reached
        if (chps.nonEmpty && wait_flag == 1) {
          stat_up(i) = 0
          stat_down(i) = 0
          if (i - chps.last <= update_width)
            wait_flag = 1
          else
            wait_flag = 0
        }

        // Update change detector parameters
        if (chps.nonEmpty) {
          if (i - chps.last == update_width) {
            val subdat = util.Arrays.copyOfRange(signal, i - update_width, i)
            mu0 = subdat.sum / subdat.length.toDouble
            mu1 = mu0 + expected_change
            mu2 = mu0 - expected_change
            k_up = (mu1 + mu0) / 2
            k_down = (mu2 + mu0) / 2
            update_points += i
          }
        }
      }
    }
    chps.toList
  }
}