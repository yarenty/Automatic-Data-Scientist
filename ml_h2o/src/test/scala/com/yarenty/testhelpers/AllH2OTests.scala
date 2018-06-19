package com.yarenty.testhelpers

import water.H2O
import water.parser.{DefaultParserProviders, ParseSetup}


object AllH2OTests {
  // Setup cloud name
  val args = Array[String]("-name", "h2oTestInstance", "--ga_opt_out")
  // Build a cloud of 1
  H2O.main(args)
  H2O.waitForCloudSize(1, 10 * 1000 /* ms */)
}

class AllH2OTests {
  val T = AllH2OTests

  def parser: ParseSetup = {
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
