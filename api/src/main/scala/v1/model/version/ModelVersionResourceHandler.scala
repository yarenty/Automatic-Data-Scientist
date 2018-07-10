package v1.model.version

import javax.inject.{Inject, Provider}

import play.api.MarkerContext

import scala.concurrent.{ExecutionContext, Future}
import play.api.libs.json._

/**
  * DTO for displaying post information.
  */
case class ModelVersionResource(id: String, link: String, title: String, body: String)

object ModelVersionResource {

  /**
    * Mapping to write a ModelVersionResource out as a JSON value.
    */
  implicit val implicitWrites = new Writes[ModelVersionResource] {
    def writes(post: ModelVersionResource): JsValue = {
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
  * Controls access to the backend data, returning [[ModelVersionResource]]
  */
class ModelVersionResourceHandler @Inject()(
    routerProvider: Provider[ModelVersionRouter],
    postRepository: ModelVersionRepository)(implicit ec: ExecutionContext) {

  def create(postInput: ModelVersionFormInput)(implicit mc: MarkerContext): Future[ModelVersionResource] = {
    val data = ModelVersionData(ModelVersionId("999"), postInput.title, postInput.body)
    // We don't actually create the post, so return what we have
    postRepository.create(data).map { id =>
      createModelVersionResource(data)
    }
  }

  def lookup(id: String)(implicit mc: MarkerContext): Future[Option[ModelVersionResource]] = {
    val postFuture = postRepository.get(ModelVersionId(id))
    postFuture.map { maybeModelVersionData =>
      maybeModelVersionData.map { postData =>
        createModelVersionResource(postData)
      }
    }
  }

  def find(implicit mc: MarkerContext): Future[Iterable[ModelVersionResource]] = {
    postRepository.list().map { postDataList =>
      postDataList.map(postData => createModelVersionResource(postData))
    }
  }

  private def createModelVersionResource(p: ModelVersionData): ModelVersionResource = {
    ModelVersionResource(p.id.toString, routerProvider.get.link(p.id), p.title, p.body)
  }

}
