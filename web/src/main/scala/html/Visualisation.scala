package html

abstract class Visualisation {

  val header =
    """
      |<!doctype html>
      |<html>
      |<head>
      |    <title>Data Visualisation </title>
      |    <script src="https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.7.2/Chart.bundle.min.js"></script>
      |    <script>
      |    'use strict';
      |
      |window.chartColors = {
      |	red: 'rgb(255, 99, 132)',
      |	redlight: 'rgb(255, 129, 162)',
      |	orange: 'rgb(255, 159, 64)',
      |	orangelight: 'rgb(255, 189, 94)',
      |	yellow: 'rgb(255, 205, 86)',
      |	yellowlight: 'rgb(255, 235, 116)',
      |	green: 'rgb(75, 192, 192)',
      |	greenlight: 'rgb(105, 222, 222)',
      |	blue: 'rgb(54, 162, 235)',
      |	bluelight: 'rgb(84, 192, 235)',
      |	purple: 'rgb(153, 102, 255)',
      |	purplelight: 'rgb(183, 132, 255)',
      |	grey: 'rgb(201, 203, 207)',
      |	greylight: 'rgb(231, 233, 237)'
      |};
      |    </script>
      |    <style>
      |    canvas {
      |        -moz-user-select: none;
      |        -webkit-user-select: none;
      |        -ms-user-select: none;
      |    }
      |    body {margin:0; padding:0; }
      |    </style>
      |</head>
      |
      |<body>
   """.stripMargin

  val footer: String =
    s"""
       |</body>
       |</html>
    """.stripMargin

}
