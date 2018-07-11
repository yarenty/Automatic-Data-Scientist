package v1.model.version

import javax.inject.Inject

import play.api.Logger
import play.api.data.Form
import play.api.libs.json.Json
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}

case class ModelVersionFormInput(title: String, body: String)

/**
  * Takes HTTP requests and produces JSON.
  */
class ModelVersionController @Inject()(cc: ModelVersionControllerComponents)(implicit ec: ExecutionContext)
    extends ModelVersionBaseController(cc) {

  private val logger = Logger(getClass)

  private val form: Form[ModelVersionFormInput] = {

    import play.api.data.Forms._
    
    Form(
      mapping(
        "title" -> nonEmptyText,
        "body" -> text
      )(ModelVersionFormInput.apply)(ModelVersionFormInput.unapply)
    )
  }

  def index: Action[AnyContent] = ModelVersionAction.async { implicit request =>
    logger.trace("index: ")
    postResourceHandler.find.map { posts =>
      Ok(Json.toJson(posts))
    }
  }

  def process: Action[AnyContent] = ModelVersionAction.async { implicit request =>
    logger.trace("process: ")
    processJsonModelVersion()
  }

  def show(id: String): Action[AnyContent] = ModelVersionAction.async { implicit request =>
    logger.trace(s"show: id = $id")
    postResourceHandler.lookup(id).map { post =>
      Ok(Json.toJson(post))
    }
  }

  private def processJsonModelVersion[A]()(implicit request: ModelVersionRequest[A]): Future[Result] = {
    def failure(badForm: Form[ModelVersionFormInput]) = {
      Future.successful(BadRequest(badForm.errorsAsJson))
    }

    def success(input: ModelVersionFormInput) = {
      postResourceHandler.create(input).map { post =>
        Created(Json.toJson(post)).withHeaders(LOCATION -> post.link)
      }
    }

    form.bindFromRequest().fold(failure, success)
  }
}
