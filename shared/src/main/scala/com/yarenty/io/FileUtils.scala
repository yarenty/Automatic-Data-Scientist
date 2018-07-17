package com.yarenty.io

import java.io.File

object FileUtils {

  def getListOfFiles(dir: String):List[String] = {
    val d = new File(dir)
    if (d.exists && d.isFile) {
      d.listFiles.filter(_.isFile).toList.map(f => f.getName)
    } else {
      List[String]()
    }
  }

  def getListOfDirs(dir: String):List[String] = {
    val d = new File(dir)
//    println(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isDirectory).toList.map(f => f.getName)
    } else {
      List[String]()
    }
  }
  
  
  def fileExist(f:String):Boolean = {
    val d = new File(f)
    d.exists && d.isFile
  }
  
  
}
