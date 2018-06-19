package com.yarenty.ml.visualisation.html

import com.yarenty.ml.utils.h2o.Helper

/** *
  * Display histograms!
  */
object HistogramCharts extends Visualisation {

  def plot(fileName: String, name:String, start:Double, binWidth:Double, histogram:Array[Int]): Unit = {
    Helper.createOutputDirectory(fileName, true)
    val d3js = header + chart(name,start, binWidth, histogram, 0) + footer
    Helper.saveString(d3js, s"$fileName.html", false)
  }


  def plot(fileName: String, hist: List[(String, Double, Double, Array[Int])]): Unit = {
    Helper.createOutputDirectory(fileName, true)
    val d3js = header + charts(hist) + footer
    Helper.saveString(d3js, s"$fileName.html", false)

  }

  def charts( hist: List[(String, Double, Double, Array[Int])]): String = {
    var jsString = new StringBuilder
    for (idx <- 0 until hist.length) {
      jsString.append(chart(hist(idx)._1,hist(idx)._2,hist(idx)._3,hist(idx)._4,  idx))
    }
    jsString.toString
  }

  //scalastyle:off
   def chart(name:String, start:Double, binWidth:Double, histogram:Array[Int], idx:Int): String = {
    val len = Math.min(Math.max(1200, 100 + histogram.length * 5), 2400)
//    val len = 800

    var jsString = new StringBuilder
    jsString.append("<canvas width=\"").append(len).append("\" height=\"600\" id=\"histogram").append(idx).append("\"></canvas>\n")

    jsString.append("<script>\n".stripMargin)
//    labels: ["${histogram.indices.map(i => (i*binWidth).toString + "-" + ((i+1)*binWidth).toString).mkString("\",\"")}"],

    jsString.append(
      s"""
        |
        |new Chart(document.getElementById("histogram${idx}"), {
        |    type: 'bar',
        |    data: {
        |      labels: [${histogram.indices.map(i => (i*binWidth) + start ).mkString(",")}],
        |      datasets: [
        |        {
        |          label: "values",
        |          data: [${histogram.mkString(",")}]
        |        },
        |       {
        |          label: "values",
        |          type: 'line',
        |          				borderColor: window.chartColors.blue,
        |				borderWidth: 2,
        |				fill: false,
        |          data: [${histogram.mkString(",")}]
        |        }
        |      ]
        |    },
        |    options: {
        |      responsive: false, hoverMode: 'index', stacked: false,
        |      legend: { display: false },
        |      title: {
        |        display: true,
        |        text: '${name}'
        |      }
        |    }
        |});
        |""".stripMargin)

    jsString.append("</script>\n".stripMargin)
    jsString.toString


  }

  //scalastyle:on


}
