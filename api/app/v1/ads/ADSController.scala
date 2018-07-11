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
class ADSController @Inject()(adsCC: ADSControllerComponents)(implicit adsEC: ExecutionContext)
    extends ADSBaseController(adsCC) {

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

  def index: Action[AnyContent] = adsAction.async { implicit request =>
    logger.trace("index: ")
    adsResourceHandler.find.map { adses =>
      Ok(Json.toJson(adses))
    }
  }

  def process: Action[AnyContent] = adsAction.async { implicit request =>
    logger.trace("process: ")
    processJsonADS()
  }

  def show(id: String): Action[AnyContent] = adsAction.async { implicit request =>
    logger.trace(s"show: id = $id")
    adsResourceHandler.lookup(id).map { ads =>
      Ok(Json.toJson(ads))
    }
  }

  private def processJsonADS[A]()(implicit adsRequest: ADSRequest[A]): Future[Result] = {
    def failure(badForm: Form[ADSFormInput]) = {
      Future.successful(BadRequest(badForm.errorsAsJson))
    }

    def success(input: ADSFormInput) = {
      adsResourceHandler.create(input).map { ads =>
        Created(Json.toJson(ads)).withHeaders(LOCATION -> ads.link)
      }
    }

    form.bindFromRequest().fold(failure, success)
  }
}
