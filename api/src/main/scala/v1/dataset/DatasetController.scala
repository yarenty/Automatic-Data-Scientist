package v1.post

import javax.inject.Inject

import play.api.Logger
import play.api.data.Form
import play.api.libs.json.Json
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}

case class DatasetFormInput(title: String, body: String)

/**
  * Takes HTTP requests and produces JSON.
  */
class DatasetController @Inject()(cc: DatasetControllerComponents)(implicit ec: ExecutionContext)
    extends DatasetBaseController(cc) {

  private val logger = Logger(getClass)

  private val form: Form[DatasetFormInput] = {
    import play.api.data.Forms._

    Form(
      mapping(
        "title" -> nonEmptyText,
        "body" -> text
      )(DatasetFormInput.apply)(DatasetFormInput.unapply)
    )
  }

  def index: Action[AnyContent] = DatasetAction.async { implicit request =>
    logger.trace("index: ")
    datasetResourceHandler.find.map { posts =>
      Ok(Json.toJson(posts))
    }
  }

  def process: Action[AnyContent] = DatasetAction.async { implicit request =>
    logger.trace("process: ")
    processJsonDataset()
  }

  def show(id: String): Action[AnyContent] = DatasetAction.async { implicit request =>
    logger.trace(s"show: id = $id")
    datasetResourceHandler.lookup(id).map { post =>
      Ok(Json.toJson(post))
    }
  }

  private def processJsonDataset[A]()(implicit request: DatasetRequest[A]): Future[Result] = {
    def failure(badForm: Form[DatasetFormInput]) = {
      Future.successful(BadRequest(badForm.errorsAsJson))
    }

    def success(input: DatasetFormInput) = {
      datasetResourceHandler.create(input).map { post =>
        Created(Json.toJson(post)).withHeaders(LOCATION -> post.link)
      }
    }

    form.bindFromRequest().fold(failure, success)
  }
}
