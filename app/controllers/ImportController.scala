package controllers

import javax.inject._
import play.api._
import play.api.mvc._

/**
  * This controller creates an `Action` to handle HTTP requests to the import files.
  */
@Singleton
class ImportController @Inject()(cc: ControllerComponents) extends AbstractController(cc) {

  def index() = Action { implicit request: Request[AnyContent] =>
    Ok("GOT Request: [" + request +"]")
  }

}
