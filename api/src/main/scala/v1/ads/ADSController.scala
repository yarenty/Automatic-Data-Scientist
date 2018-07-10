package v1.ads

import javax.inject.Inject

import play.api.Logger
import play.api.data.Form
import play.api.libs.json.Json
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}

case class ADSFormInput(title: String, body: String)

/**
  * Takes HTTP requests and produces JSON.
  */
class ADSController @Inject()(cc: ADSControllerComponents)(implicit ec: ExecutionContext)
    extends ADSBaseController(cc) {

  private val logger = Logger(getClass)

  private val form: Form[ADSFormInput] = {

    import play.api.data.Forms._
    
    Form(
      mapping(
        "title" -> nonEmptyText,
        "body" -> text
      )(ADSFormInput.apply)(ADSFormInput.unapply)
    )
  }

  def index: Action[AnyContent] = ADSAction.async { implicit request =>
    logger.trace("index: ")
    postResourceHandler.find.map { posts =>
      Ok(Json.toJson(posts))
    }
  }

  def process: Action[AnyContent] = ADSAction.async { implicit request =>
    logger.trace("process: ")
    processJsonADS()
  }

  def show(id: String): Action[AnyContent] = ADSAction.async { implicit request =>
    logger.trace(s"show: id = $id")
    postResourceHandler.lookup(id).map { post =>
      Ok(Json.toJson(post))
    }
  }

  private def processJsonADS[A]()(implicit request: ADSRequest[A]): Future[Result] = {
    def failure(badForm: Form[ADSFormInput]) = {
      Future.successful(BadRequest(badForm.errorsAsJson))
    }

    def success(input: ADSFormInput) = {
      postResourceHandler.create(input).map { post =>
        Created(Json.toJson(post)).withHeaders(LOCATION -> post.link)
      }
    }

    form.bindFromRequest().fold(failure, success)
  }
}
