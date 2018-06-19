package com.yarenty.ml.algorithms.timeseries

import breeze.numerics.{pow, sqrt}

/**
  * Currently there are 3 algorithms:
  * - using 3 methods (check what)
  * - using Eulers distance on RRC.Success.Rate (Traffic.User.Avg?) - one dimension
  * - PCA - wip
  *
  * Eulers - one dimension
  * normalize data - sigmoid function
  * kNN
  * dist = lambda * sqrt( (dist&#94;2) )
  * if big ~ abnormal
  *
  * (C)2018 by yarenty
  */
object AbnormalDetector {


  def findNearestClasses(testPoints: Array[Array[Double]], trainPoints: Array[Array[Double]]): Array[Int] = {
    testPoints.map { testInstance =>
      trainPoints.zipWithIndex.map {
        case (trainInstance, c) =>
          c -> distance(testInstance, trainInstance)
      }.minBy(_._2)._1
    }
  }

  def distance(xs: Array[Double], ys: Array[Double]): Double = {
    sqrt((xs zip ys).map { case (x, y) => pow(y - x, 2) }.sum)
  }

  def findKNearestClasses(testPoints: Array[Array[Double]], trainPoints: Array[Array[Double]], k: Int): Array[Int] = {
    testPoints.map { testInstance =>
      val distances =
        trainPoints.zipWithIndex.map {
          case (trainInstance, c) =>
            c -> distance(testInstance, trainInstance)
        }
      val classes = distances.sortBy(_._2).take(k).map(_._1)
      val classCounts = classes.groupBy(identity).mapValues(_.length)
      classCounts.maxBy(_._2)._1
    }
  }


}
