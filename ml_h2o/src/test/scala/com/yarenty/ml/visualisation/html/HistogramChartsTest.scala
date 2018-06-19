package com.yarenty.ml.visualisation.html

import com.yarenty.testhelpers.{TestHelper, TimeSeriesData}
import com.yarenty.ml.data.distribution.{Cauchy, Histogram, Normal}
import org.junit.Assert.{assertArrayEquals, assertEquals}
import org.junit.Test

class HistogramChartsTest {


  @Test
  def testHistogram(): Unit = {
    val histogram = new Histogram(TimeSeriesData.data)

    val hist = histogram.default
    val binWidth = histogram.getBinWidth

    assertEquals(8, hist.length)
    assertArrayEquals(Array(21, 45, 40, 35, 11, 7, 7, 2), hist)
    HistogramCharts.plot( TestHelper.generateFileName("histogram/default"), "Default histogram", TimeSeriesData.data.min, binWidth, hist)
  }


  @Test
  def testHistogramGaussian100(): Unit = {
    val data = new Normal().getSampleData(1000)
    val histogram = new Histogram(data)

    val hist = histogram.byNumberOfBins(100)
    val binWidth = histogram.getBinWidth

    HistogramCharts.plot( TestHelper.generateFileName("histogram/Gaussian100"), "Gaussian histogram (100bins)", data.min, binWidth, hist)
  }

  @Test
  def testHistogramGaussian(): Unit = {
    val data = new Normal().getSampleData(1000)
    val histogram = new Histogram(data)

    val hist = histogram.default
    val binWidth = histogram.getBinWidth

    HistogramCharts.plot( TestHelper.generateFileName("histogram/Gaussian"), "Gaussian histogram",data.min, binWidth, hist)
  }




  @Test
  def testHistogramCauchy(): Unit = {
    val data = new Cauchy().getSampleData(1000)
    println(data.min)
    println(data.max)
    val histogram = new Histogram(data)

    val hist = histogram.byNumberOfBins(100)
    val binWidth = histogram.getBinWidth

    HistogramCharts.plot( TestHelper.generateFileName("histogram/Cauchy"), "Cauchy histogram", data.min, binWidth, hist)

  }

  @Test
  def testMultipleHistograms(): Unit = {
    val histogram1 = new Histogram(TimeSeriesData.data)
    val hist1 = histogram1.default
    val binWidth1 = histogram1.getBinWidth


    val histogram2 = new Histogram(TimeSeriesData.data)
    val hist2 = histogram2.byNumberOfBins(16)
    val binWidth2 = histogram2.getBinWidth

    val histogram3 = new Histogram(TimeSeriesData.data)
    val hist3 = histogram3.byBinWidth(30)
    val binWidth3 = histogram3.getBinWidth

    val histogram4 = new Histogram(TimeSeriesData.data)
    val hist4 = histogram4.byNumberOfBins(TimeSeriesData.data.length/10)
    val binWidth4 = histogram4.getBinWidth

    val histograms = List(
      ("Default histogram (Scotts Rule)",TimeSeriesData.data.min,binWidth1,hist1),
      ("16 bins histogram",TimeSeriesData.data.min,binWidth2,hist2),
      ("Bins width of 30 histogram",TimeSeriesData.data.min,binWidth3,hist3),
      ("Data.length/10 histogram",TimeSeriesData.data.min,binWidth4,hist4)
      )

    HistogramCharts.plot( TestHelper.generateFileName("histogram/multipleHistograms"), histograms)

  }


}
