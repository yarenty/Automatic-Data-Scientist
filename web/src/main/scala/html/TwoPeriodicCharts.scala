package html

import com.yarenty.ml.utils.h2o.Helper

/** *
  * To display two lines based on time serie
  */
object TwoPeriodicCharts extends Visualisation {

  def plot(name: String, timeStamp: Array[String],
           first: Array[Double], firstName: String,
           second: Array[Double], secondName: String,
           scaled:Boolean = false
          ): Unit = {
    //Create dir
    Helper.createOutputDirectory(name, true)
    //copyJsLib(name)
    //val anomalies = cps2Anomaly
    val d3js = header + chart(timeStamp, first, firstName, second, secondName, scaled) + footer
    Helper.saveString(d3js, s"$name.html", false)

  }

//scalastyle:off
  private def chart(ts: Array[String], first: Array[Double], firstName: String,
                    second: Array[Double], secondName: String,  scaled: Boolean = false): String = {
    val len = Math.min(Math.max(1200, 100 + ts.length * 5), 2400)

    val min: Double = Math.min(first.min,second.min)
    val max: Double = Math.max(first.max,second.max)

    var jsString = new StringBuilder
    jsString.append("<canvas width=\"").append(len).append("\" height=\"800\" id=\"canvas\"></canvas>\n")

    jsString.append("<script>\n var lineChartData = {\nlabels: [".stripMargin)
    jsString.append("\"").append(ts.mkString("\",\"")).append("\"")
    jsString.append("], \ndatasets: [{ \nlabel: \"").append(firstName).append("\",")
    jsString.append(
      """
        | borderColor: window.chartColors.red, backgroundColor: window.chartColors.redlight,
        | borderWidth:1.5,pointRadius:1.2, fill: false,
        | fill: false,
        | data: [""".stripMargin)

    jsString.append(first.mkString(",").replace("NaN", ""))
    jsString.append("], yAxisID: \"y-axis-1\",}, { label: \"").append(secondName).append("\",")
    jsString.append(
      """ 
        | borderColor: window.chartColors.blue, backgroundColor: window.chartColors.bluelight,
        | borderWidth:1.2, pointRadius: 1,
        | borderDash: [3, 5], fill: false,
        |data: [""".stripMargin)
    jsString.append(second.mkString(",").replace("NaN", ""))
    jsString.append("], yAxisID: \"y-axis-2\"}")


    jsString.append("]};")

    jsString.append(
      s"""
       |  window.onload = function() {
       |    var ctx = document.getElementById("canvas").getContext("2d");
       |    window.myLine = Chart.Line(ctx, { data: lineChartData,
       |      options: { responsive: false, hoverMode: 'index', stacked: false,
       |        legend: {labels: {usePointStyle: true }},
       |        elements: { point: { pointStyle: 'circle' } },
       |        scales: {
       |         xAxes: [{ display: true, scaleLabel: {display: true, labelString: 'Time'} }],
       |
         | yAxes: [{
         |            type: "linear",
         |            display: true,
         |            position: "left",
         |            id: "y-axis-1",
        """.stripMargin)
    if (scaled) jsString.append(s"ticks: {min:   ${min}, max:   ${max}},")
     jsString.append(
    s"""
         |            scaleLabel: {
         |              display: true,
         |              fontColor:window.chartColors.red,
         |              labelString: '${firstName}'
         |            }
         |          },
         |          {
         |            type: "linear",
         |            display: true,
         |            position: "right",
         |            id: "y-axis-2",
       """.stripMargin)
    if (scaled) jsString.append(s"ticks: {min:   ${min}, max:   ${max}},")
    jsString.append(
      s"""
         |            fontColor: "#0000ff",
         |            gridLines: {
         |              drawOnChartArea: false,
         |            },
         |            scaleLabel: {
         |              fontColor:window.chartColors.blue,
         |              display: true,
         |              labelString: '${secondName}'
         |            }
         |          }],
         |        }
         |      }
         |    });
         |  };
         |</script>
    """.stripMargin
    )
    jsString.toString


  }

  //scalastyle:on


}
