package v1.ads



import javax.inject.Inject

import play.api.routing.Router.Routes
import play.api.routing.SimpleRouter
import play.api.routing.sird._
/**
  * Routes and URLs to the ADSResource controller.
  */
class ADSRouter @Inject()(controller: ADSController) extends SimpleRouter {
  val prefix = "/v1/posts"

  def link(id: ADSId): String = {
    import com.netaporter.uri.dsl._
    val url = prefix / id.toString
    url.toString()
  }

  override def routes: Routes = {
    case GET(p"/") =>
      controller.index

    case POST(p"/") =>
      controller.process

    case GET(p"/$id") =>
      controller.show(id)
  }

}
