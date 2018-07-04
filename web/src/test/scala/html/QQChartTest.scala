package html

import com.yarenty.ml.data.distribution._
import org.apache.commons.math3.stat.descriptive.rank.Percentile
import org.junit.Test
import com.yarenty.testhelpers._

class QQChartTest {


  @Test
  def testQQ(): Unit = {
    val p1 = new Percentile()
    p1.setData(TimeSeriesData.data)

    val p2 = new Percentile()
    p2.setData(new Normal().getSampleData(TimeSeriesData.data.length))

    val q1 = Array.ofDim[Double](100)
    val q2 = Array.ofDim[Double](100)

    for (i <- 1 to 100) {
      q1(i - 1) = {
        p1.setQuantile(i); p1.evaluate()
      }
      q2(i - 1) = {
        p2.setQuantile(i); p2.evaluate()
      }
    }

    QQCharts.plot(TestHelper.generateFileName("qq/GaussianChart"), "data over gaussian", ("Distribution: Gaussian", "input data"), (q2, q1))

  }


  @Test
  def testQQCauchy(): Unit = {
    val p1 = new Percentile()
    p1.setData(TimeSeriesData.data)

    val p2 = new Percentile()
    p2.setData(new Cauchy().getSampleData(TimeSeriesData.data.length))

    val q1 = Array.ofDim[Double](100)
    val q2 = Array.ofDim[Double](100)

    for (i <- 1 to 100) {
      q1(i - 1) = {
        p1.setQuantile(i); p1.evaluate()
      }
      q2(i - 1) = {
        p2.setQuantile(i); p2.evaluate()
      }
    }

    QQCharts.plot(TestHelper.generateFileName("qq/CauchyChart"), "data over Cauchy", ("Distribution: Cauchy", "input data"), (q2, q1))

  }


  @Test
  def testQQExp(): Unit = {
    val p1 = new Percentile()
    p1.setData(TimeSeriesData.data)

    val p2 = new Percentile()
    p2.setData(new Exponential(800).getSampleData(TimeSeriesData.data.length))

    val q1 = Array.ofDim[Double](100)
    val q2 = Array.ofDim[Double](100)

    for (i <- 1 to 100) {
      q1(i - 1) = {
        p1.setQuantile(i); p1.evaluate()
      }
      q2(i - 1) = {
        p2.setQuantile(i); p2.evaluate()
      }
    }

    QQCharts.plot(TestHelper.generateFileName("qq/ExponentialChart"), "data over Exponential(800)", ("Distribution: Exponential(800)", "input data"), (q2, q1))

  }

  @Test
  def testQQChi1(): Unit = {
    val p1 = new Percentile()
    p1.setData(TimeSeriesData.data)

    val p2 = new Percentile()
    p2.setData(new ChiSquared(1).getSampleData(TimeSeriesData.data.length))

    val q1 = Array.ofDim[Double](100)
    val q2 = Array.ofDim[Double](100)

    for (i <- 1 to 100) {
      q1(i - 1) = {
        p1.setQuantile(i); p1.evaluate()
      }
      q2(i - 1) = {
        p2.setQuantile(i); p2.evaluate()
      }
    }

    QQCharts.plot(TestHelper.generateFileName("qq/Chi1Chart"), "data over ChiSquared(1)", ("Distribution: ChiSquare(1)", "input data"), (q2, q1))
  }


  @Test
  def testQQChi2(): Unit = {
    val p1 = new Percentile()
    p1.setData(TimeSeriesData.data)

    val p2 = new Percentile()
    p2.setData(new ChiSquared(2).getSampleData(TimeSeriesData.data.length))

    val q1 = Array.ofDim[Double](100)
    val q2 = Array.ofDim[Double](100)

    for (i <- 1 to 100) {
      q1(i - 1) = {
        p1.setQuantile(i); p1.evaluate()
      }
      q2(i - 1) = {
        p2.setQuantile(i); p2.evaluate()
      }
    }

    QQCharts.plot(TestHelper.generateFileName("qq/Chi2Chart"), "data over ChiSquared(2)", ("Distribution: ChiSquare(2)", "input data"), (q2, q1))
  }


  @Test
  def testQQChi3(): Unit = {
    val p1 = new Percentile()
    p1.setData(TimeSeriesData.data)

    val p2 = new Percentile()
    p2.setData(new ChiSquared(3).getSampleData(TimeSeriesData.data.length))

    val q1 = Array.ofDim[Double](100)
    val q2 = Array.ofDim[Double](100)

    for (i <- 1 to 100) {
      q1(i - 1) = {
        p1.setQuantile(i); p1.evaluate()
      }
      q2(i - 1) = {
        p2.setQuantile(i); p2.evaluate()
      }
    }

    QQCharts.plot(TestHelper.generateFileName("qq/Chi3Chart"), "data over ChiSquared(3)", ("Distribution: ChiSquare(3)", "input data"), (q2, q1))
  }




  @Test
  def testQQCauchyCauchy(): Unit = {
    val p1 = new Percentile()
    p1.setData(new Cauchy().getSampleData(1000))

    val p2 = new Percentile()
    p2.setData(new Cauchy().getSampleData(1000))

    val q1 = Array.ofDim[Double](100)
    val q2 = Array.ofDim[Double](100)

    for (i <- 1 to 100) {
      q1(i - 1) = {
        p1.setQuantile(i); p1.evaluate()
      }
      q2(i - 1) = {
        p2.setQuantile(i); p2.evaluate()
      }
    }

    QQCharts.plot(TestHelper.generateFileName("qq/CauchyCachyChart"), "Cauchy over Cauchy", ("Distribution: Cauchy", "Distribution: Cauchy"), (q2, q1))

  }



  @Test
  def testQQMultiple(): Unit = {
    val p1 = new Percentile()
    p1.setData(TimeSeriesData.data)
    val p2 = new Percentile()
    p2.setData(new Normal().getSampleData(TimeSeriesData.data.length))
    val p3 = new Percentile()
    p3.setData(new Normal().getSampleData(1000))

    val q1 = Array.ofDim[Double](100)
    val q2 = Array.ofDim[Double](100)
    val q3 = Array.ofDim[Double](100)

    for (i <- 1 to 100) {
      q1(i - 1) = {
        p1.setQuantile(i); p1.evaluate()
      }
      q2(i - 1) = {
        p2.setQuantile(i); p2.evaluate()
      }

      q3(i - 1) = {
        p3.setQuantile(i); p3.evaluate()
      }
    }

    val multi = List( ("data over gaussian", ("Distribution: Gaussian", "input data"), (q2, q1)),
      ("gaussian over gaussian", ("Distribution: Gaussian", "Distribution: Gaussian"), (q2, q3)) )

    QQCharts.plot(TestHelper.generateFileName("qq/GaussianMultipleChart"), multi)

  }



}
