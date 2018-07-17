package com.yarenty.ml.preprocessing

import water.parser.{DefaultParserProviders, ParseSetup}

object EmptyCSVParser {


  def get: ParseSetup = {
    val p: ParseSetup = new ParseSetup()
    //      val names: Array[String] =
    //      val types = ParseSetup.strToColumnTypes(Array(")) //21
    //      parseOrders.setColumnNames(names)
    //      parseOrders.setColumnTypes(types)
    p.setParseType(DefaultParserProviders.CSV_INFO)
    //      parseOrders.setNumberColumns(101)
    //      parseOrders.setNAStrings(Array.fill(101)(Array("NIL")))
    p.setSeparator(44)  // scalastyle:ignore magic number ;-)
    p.setSingleQuotes(false)
    p.setCheckHeader(1)
    p
  }

}