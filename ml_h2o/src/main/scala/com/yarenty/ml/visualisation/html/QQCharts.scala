package com.yarenty.ml.visualisation.html

import com.yarenty.ml.utils.h2o.Helper

object QQCharts extends Visualisation {


  def plot(fileName: String, name: String, xyLabels: (String, String), xy: (Array[Double], Array[Double])): Unit = {
    Helper.createOutputDirectory(fileName, true)
    val d3js = header + chart(name, xyLabels, xy) + footer
    Helper.saveString(d3js, s"$fileName.html", false)
  }

  def plot(fileName: String, charts:List[(String, (String, String), (Array[Double], Array[Double]))]): Unit = {
    Helper.createOutputDirectory(fileName, true)
    val d3js = header + chart(charts) + footer
    Helper.saveString(d3js, s"$fileName.html", false)
  }


  private def chart(charts:List[(String, (String, String), (Array[Double], Array[Double]))]): String = {
    var jsString = new StringBuilder
    for(i <- charts.indices) {
      jsString.append(chart(charts(i)._1,charts(i)._2,charts(i)._3,i))
    }
    jsString.toString
  }


  //scalastyle:off
  private def chart(name: String, xyLabels: (String, String), xy: (Array[Double], Array[Double]), idx:Int = 0): String = {


    val len = 600 //best are squared

    var jsString = new StringBuilder
    jsString.append("<canvas width=\"").append(len).append("\" height=\"").append(len).append("\" id=\"QQ").append(idx).append("\"></canvas>\n")

    jsString.append("<script>\n".stripMargin)

    jsString.append(
      s"""
         | var scatterChartData = {
         |  datasets: [{
         |     label: "${name}",
         |     xAxisID: 'x-axis-1-${idx}',
         |  	 yAxisID: 'y-axis-1-${idx}',
         |     borderColor: window.chartColors.red,
         |		 data: [
         """.stripMargin)

    for (i <- 0 until xy._1.length) {
      jsString.append("{x:").append(xy._1(i)).append(",	y:").append(xy._2(i)).append(",},")
    }

    jsString.append(
      s"""
         | ] }]
         | };
         |
         |new Chart.Scatter(document.getElementById("QQ${idx}"), {
         |
         |  	data: scatterChartData,
         |    options: {
         |      responsive: false, hoverMode: 'nearest', stacked: false,
         |      intersect: true,
         |      legend: { display: true },
         |      title: { display: true, text: 'QQ' },
         |
         |      scales: {
         |					xAxes: [{	position: 'bottom',
         |          scaleLabel: { display:true, labelString: '${xyLabels._1}'},
         |      	  gridLines: {zeroLineColor: 'rgba(0,0,0,1)' },
         |          id: 'x-axis-1-${idx}',
         |					}],
         |
         |					yAxes: [{
         |						type: 'linear', // only linear but allow scale type registration. This allows extensions to exist solely for log scale for instance
         |						display: true,
         |          scaleLabel: { display:true, labelString: '${xyLabels._2}'},
         |       					position: 'left',
         |						id: 'y-axis-1-${idx}',
         |					}]
         |				} //scales
         |
         |    }//options

         |}); //onload
         """.stripMargin)

    jsString.append("</script>\n".stripMargin)
    jsString.toString


  }

  //scalastyle:on

}
