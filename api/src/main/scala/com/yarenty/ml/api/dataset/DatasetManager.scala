package com.yarenty.ml.api.dataset

import com.yarenty.ml.api.types.Dataset

class DatasetManager {
  
  val dses:Map[Int, Dataset] = ???
  
  
  def create():Int = ???  // create new dataset
  def patch(id:Int) = ??? //update new dataset
  def delete(id:Int) = ??? // delete dataset
  
  
  
  def list() = ??? //get list of dataset

  def slices(id:Int) = ??? // get slices of dataset for processing
  def slices(id:Int, names:Array[String], dividers:Array[Double]) = ??? // get slices of dataset for processing
  
}
