package v1.service.anomaly

import javax.inject.{Inject, Provider}

import play.api.MarkerContext

import scala.concurrent.{ExecutionContext, Future}
import play.api.libs.json._

/**
  * DTO for displaying post information.
  */
case class AnomalyServiceResource(id: String, link: String, title: String, body: String)

object AnomalyServiceResource {

  /**
    * Mapping to write a AnomalyServiceResource out as a JSON value.
    */
  implicit val implicitWrites = new Writes[AnomalyServiceResource] {
    def writes(post: AnomalyServiceResource): JsValue = {
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
  * Controls access to the backend data, returning [[AnomalyServiceResource]]
  */
class AnomalyServiceResourceHandler @Inject()(
    routerProvider: Provider[AnomalyServiceRouter],
    postRepository: AnomalyServiceRepository)(implicit ec: ExecutionContext) {

  def create(postInput: AnomalyServiceFormInput)(implicit mc: MarkerContext): Future[AnomalyServiceResource] = {
    val data = AnomalyServiceData(AnomalyServiceId("999"), postInput.title, postInput.body)
    // We don't actually create the post, so return what we have
    postRepository.create(data).map { id =>
      createAnomalyServiceResource(data)
    }
  }

  def lookup(id: String)(implicit mc: MarkerContext): Future[Option[AnomalyServiceResource]] = {
    val postFuture = postRepository.get(AnomalyServiceId(id))
    postFuture.map { maybeAnomalyServiceData =>
      maybeAnomalyServiceData.map { postData =>
        createAnomalyServiceResource(postData)
      }
    }
  }

  def find(implicit mc: MarkerContext): Future[Iterable[AnomalyServiceResource]] = {
    postRepository.list().map { postDataList =>
      postDataList.map(postData => createAnomalyServiceResource(postData))
    }
  }

  private def createAnomalyServiceResource(p: AnomalyServiceData): AnomalyServiceResource = {
    AnomalyServiceResource(p.id.toString, routerProvider.get.link(p.id), p.title, p.body)
  }

}
