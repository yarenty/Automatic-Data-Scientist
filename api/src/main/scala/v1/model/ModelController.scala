package v1.model

import javax.inject.Inject

import play.api.Logger
import play.api.data.Form
import play.api.libs.json.Json
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}

case class ModelFormInput(title: String, body: String)

/**
  * Takes HTTP requests and produces JSON.
  */
class ModelController @Inject()(cc: ModelControllerComponents)(implicit ec: ExecutionContext)
    extends ModelBaseController(cc) {

  private val logger = Logger(getClass)

  private val form: Form[ModelFormInput] = {

    import play.api.data.Forms._
    
    Form(
      mapping(
        "title" -> nonEmptyText,
        "body" -> text
      )(ModelFormInput.apply)(ModelFormInput.unapply)
    )
  }

  def index: Action[AnyContent] = ModelAction.async { implicit request =>
    logger.trace("index: ")
    postResourceHandler.find.map { posts =>
      Ok(Json.toJson(posts))
    }
  }

  def process: Action[AnyContent] = ModelAction.async { implicit request =>
    logger.trace("process: ")
    processJsonModel()
  }

  def show(id: String): Action[AnyContent] = ModelAction.async { implicit request =>
    logger.trace(s"show: id = $id")
    postResourceHandler.lookup(id).map { post =>
      Ok(Json.toJson(post))
    }
  }

  private def processJsonModel[A]()(implicit request: ModelRequest[A]): Future[Result] = {
    def failure(badForm: Form[ModelFormInput]) = {
      Future.successful(BadRequest(badForm.errorsAsJson))
    }

    def success(input: ModelFormInput) = {
      postResourceHandler.create(input).map { post =>
        Created(Json.toJson(post)).withHeaders(LOCATION -> post.link)
      }
    }

    form.bindFromRequest().fold(failure, success)
  }
}
