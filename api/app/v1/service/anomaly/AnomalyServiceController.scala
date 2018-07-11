package v1.service.anomaly

import javax.inject.Inject

import play.api.Logger
import play.api.data.Form
import play.api.libs.json.Json
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}

case class AnomalyServiceFormInput(title: String, body: String)

/**
  * Takes HTTP requests and produces JSON.
  */
class AnomalyServiceController @Inject()(cc: AnomalyServiceControllerComponents)(implicit ec: ExecutionContext)
    extends AnomalyServiceBaseController(cc) {

  private val logger = Logger(getClass)

  private val form: Form[AnomalyServiceFormInput] = {
    import play.api.data.Forms._
    
    Form(
      mapping(
        "title" -> nonEmptyText,
        "body" -> text
      )(AnomalyServiceFormInput.apply)(AnomalyServiceFormInput.unapply)
    )
  }

  def index: Action[AnyContent] = AnomalyServiceAction.async { implicit request =>
    logger.trace("index: ")
    postResourceHandler.find.map { posts =>
      Ok(Json.toJson(posts))
    }
  }

  def process: Action[AnyContent] = AnomalyServiceAction.async { implicit request =>
    logger.trace("process: ")
    processJsonAnomalyService()
  }

  def show(id: String): Action[AnyContent] = AnomalyServiceAction.async { implicit request =>
    logger.trace(s"show: id = $id")
    postResourceHandler.lookup(id).map { post =>
      Ok(Json.toJson(post))
    }
  }

  private def processJsonAnomalyService[A]()(implicit request: AnomalyServiceRequest[A]): Future[Result] = {
    def failure(badForm: Form[AnomalyServiceFormInput]) = {
      Future.successful(BadRequest(badForm.errorsAsJson))
    }

    def success(input: AnomalyServiceFormInput) = {
      postResourceHandler.create(input).map { post =>
        Created(Json.toJson(post)).withHeaders(LOCATION -> post.link)
      }
    }

    form.bindFromRequest().fold(failure, success)
  }
}
