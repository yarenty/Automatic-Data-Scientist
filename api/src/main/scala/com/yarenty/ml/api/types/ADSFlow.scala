package com.yarenty.ml.api.types

import com.yarenty.ml.api.JsonEncoder.{AutoSerializable, _}
/**
  * (C)2018 
  * @author yarenty
  */


/**
  * Contains information about initial train/test/validation datasets
  * @param train
  * @param test
  * @param valid
  */
case class Dataset(
                  train:String,
                  test: String,
                  valid:String
                  )

/**
  * List of possible transformations
  * @param algorithm
  * @param params
  */
case class Transformation(
                         algorithm:String,
                         params:List[String]
                         )


/**
  * What types of AFE should be performed - default all.
  * how many levels - default 3.
  * @param levels
  * @param allTransformations
  * @param transformations
  */
case class AFE(
              levels:Int = 3,
              allTransformations:Boolean = true,
              transformations:List[Transformation]
              )

/**
  * Types of available algorithms
  * @param name
  * @param hyperParameters
  */
case class Algorithm(
                    name:String,
                    hyperParameters:List[String]
                    )

/**
  * What algorithms should be processed:
  * default -all
  * @param allAlgorithms
  * @param objective
  * @param algorithms
  */
case class Algorithms(
                     allAlgorithms:Boolean = true,
                     objective:String,
                     algorithms:List[Algorithm]
                     )

case class ADSFlow (
                   name:String,
                   id:Int,
                   status:String,
                   dataset: Dataset,
                   afe: AFE,
                   algorithms: Algorithms,
                   validation:String, //lime/loco
                   error:Option[Boolean] = None
                   ) extends AutoSerializable



