package v1.service.prediction

import javax.inject.Inject

import play.api.Logger
import play.api.data.Form
import play.api.libs.json.Json
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}

case class PredictionServiceFormInput(title: String, body: String)

/**
  * Takes HTTP requests and produces JSON.
  */
class PredictionServiceController @Inject()(cc: PredictionServiceControllerComponents)(implicit ec: ExecutionContext)
    extends PredictionServiceBaseController(cc) {

  private val logger = Logger(getClass)

  private val form: Form[PredictionServiceFormInput] = {
    import play.api.data.Forms._
    
    Form(
      mapping(
        "title" -> nonEmptyText,
        "body" -> text
      )(PredictionServiceFormInput.apply)(PredictionServiceFormInput.unapply)
    )
  }

  def index: Action[AnyContent] = PredictionServiceAction.async { implicit request =>
    logger.trace("index: ")
    postResourceHandler.find.map { posts =>
      Ok(Json.toJson(posts))
    }
  }

  def process: Action[AnyContent] = PredictionServiceAction.async { implicit request =>
    logger.trace("process: ")
    processJsonPredictionService()
  }

  def show(id: String): Action[AnyContent] = PredictionServiceAction.async { implicit request =>
    logger.trace(s"show: id = $id")
    postResourceHandler.lookup(id).map { post =>
      Ok(Json.toJson(post))
    }
  }

  private def processJsonPredictionService[A]()(implicit request: PredictionServiceRequest[A]): Future[Result] = {
    def failure(badForm: Form[PredictionServiceFormInput]) = {
      Future.successful(BadRequest(badForm.errorsAsJson))
    }

    def success(input: PredictionServiceFormInput) = {
      postResourceHandler.create(input).map { post =>
        Created(Json.toJson(post)).withHeaders(LOCATION -> post.link)
      }
    }

    form.bindFromRequest().fold(failure, success)
  }
}
