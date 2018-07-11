package v1.service.prediction

import javax.inject.{Inject, Provider}

import play.api.MarkerContext

import scala.concurrent.{ExecutionContext, Future}
import play.api.libs.json._

/**
  * DTO for displaying post information.
  */
case class PredictionServiceResource(id: String, link: String, title: String, body: String)

object PredictionServiceResource {

  /**
    * Mapping to write a PredictionServiceResource out as a JSON value.
    */
  implicit val implicitWrites = new Writes[PredictionServiceResource] {
    def writes(post: PredictionServiceResource): JsValue = {
      Json.obj(
        "id" -> post.id,
        "link" -> post.link,
        "title" -> post.title,
        "body" -> post.body
      )
    }
  }
}

/**
  * Controls access to the backend data, returning [[PredictionServiceResource]]
  */
class PredictionServiceResourceHandler @Inject()(
    routerProvider: Provider[PredictionServiceRouter],
    postRepository: PredictionServiceRepository)(implicit ec: ExecutionContext) {

  def create(postInput: PredictionServiceFormInput)(implicit mc: MarkerContext): Future[PredictionServiceResource] = {
    val data = PredictionServiceData(PredictionServiceId("999"), postInput.title, postInput.body)
    // We don't actually create the post, so return what we have
    postRepository.create(data).map { id =>
      createPredictionServiceResource(data)
    }
  }

  def lookup(id: String)(implicit mc: MarkerContext): Future[Option[PredictionServiceResource]] = {
    val postFuture = postRepository.get(PredictionServiceId(id))
    postFuture.map { maybePredictionServiceData =>
      maybePredictionServiceData.map { postData =>
        createPredictionServiceResource(postData)
      }
    }
  }

  def find(implicit mc: MarkerContext): Future[Iterable[PredictionServiceResource]] = {
    postRepository.list().map { postDataList =>
      postDataList.map(postData => createPredictionServiceResource(postData))
    }
  }

  private def createPredictionServiceResource(p: PredictionServiceData): PredictionServiceResource = {
    PredictionServiceResource(p.id.toString, routerProvider.get.link(p.id), p.title, p.body)
  }

}
