package controllers

import javax.inject._
import play.api._
import play.api.mvc._
//import com.yarenty.io.FileUtils

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class ADSController @Inject()(cc: ControllerComponents) extends AbstractController(cc) {

  /**
   * Create an Action to render an HTML page.
   *
   * The configuration in the `routes` file means that this method
   * will be called when the application receives a `GET` request with
   * a path of `/`.
   */
  def index() = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.ads_index())
  }

  def data() = Action { implicit request: Request[AnyContent] =>
    Ok("GOT Request: [" + request +"]")
    
    val d = if (request == null ) "/opt/data" else request
//    val dirs = FileUtils.getListofDirs(d)
//    val files = FileUtils.getListofFiles(d)
    
//    Ok(views.html.ads_data(dirs,files))
    Ok(views.html.files())
  }

  def afe() = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.files())
  }

  def algo() = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.files())
  }

  def lime() = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.files())
  }

  def loco() = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.files())
  }



}
