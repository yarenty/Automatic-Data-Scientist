package controllers

import com.yarenty.ml.data.distribution.{Exponential, F, Gamma, Gumbel, Laplace, Levy, Logistic, Nakagami, Normal, _}
import com.yarenty.ml.visualisation.html.HistogramCharts
import javax.inject.{Inject, Singleton}
import play.api.mvc._
import play.twirl.api.Html
import play.{Application, Play}
import org.apache.commons.io.IOUtils

/**
  * Distributions: info, examples + processing.
  * @param cc
  * (C)2018 by yarenty
  */
@Singleton
class DistributionController @Inject()(cc: ControllerComponents) extends AbstractController(cc) {

  def index() = Action { implicit request: Request[AnyContent] =>
    if (request.queryString.contains("d")) {

      val dist:String = request.queryString.get("d").get.mkString(";; ")

      val chart = dist match {
        case "beta" => new Beta(5,1)
        case "gaussian" => new Normal
        case "cauchy" => new Cauchy(0,0.5)
        case "chi-squared" => new ChiSquared(1)
        case "exponential" => new Exponential(1)
        case "f" => new F(2,1)
        case "gamma" => new Gamma(1,2)
        case "gumbel" => new Gumbel(2,1)
        case "laplace" =>new Laplace(0,1)
        case "levy" =>  new Levy(0, 1)
        case "logistic" => new Logistic(5,2)
        case "logNormal" => new LogNormal
        case "nakagami" => new Nakagami(5,1)
        case "pareto" => new Pareto
        case "t" => new T
        case "tiangular" => new Triangular(1,3,4)
        case "uniform" => new UniformReal(1,2)
        case "weibull" => new Weibull
        case _ => new Normal
      }
      Ok(views.html.distribution(dist.toUpperCase, Html(getDescription(dist)),dist+".png",getChart(chart)))
    } else {
      Ok(views.html.distributionlist())
    }

  }

  private def getChart(dist: RealDistribution) : Html = {
    val data = dist.getSampleData(1000)
    val histogram = new Histogram(data)
    val hist = histogram.default
    val binWidth = histogram.getBinWidth
    Html(HistogramCharts.chart("Histogram", data.min, binWidth, hist,0))
  }

  def getDescription(file:String):String = {
    IOUtils.toString(Play.application().resourceAsStream("/public/static/distribution/"+file+".html"))
  }

}
