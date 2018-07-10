package v1.model

import javax.inject.{Inject, Provider}

import play.api.MarkerContext

import scala.concurrent.{ExecutionContext, Future}
import play.api.libs.json._

/**
  * DTO for displaying post information.
  */
case class ModelResource(id: String, link: String, title: String, body: String)

object ModelResource {

  /**
    * Mapping to write a ModelResource out as a JSON value.
    */
  implicit val implicitWrites = new Writes[ModelResource] {
    def writes(post: ModelResource): JsValue = {
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
  * Controls access to the backend data, returning [[ModelResource]]
  */
class ModelResourceHandler @Inject()(
    routerProvider: Provider[ModelRouter],
    postRepository: ModelRepository)(implicit ec: ExecutionContext) {

  def create(postInput: ModelFormInput)(implicit mc: MarkerContext): Future[ModelResource] = {
    val data = ModelData(ModelId("999"), postInput.title, postInput.body)
    // We don't actually create the post, so return what we have
    postRepository.create(data).map { id =>
      createModelResource(data)
    }
  }

  def lookup(id: String)(implicit mc: MarkerContext): Future[Option[ModelResource]] = {
    val postFuture = postRepository.get(ModelId(id))
    postFuture.map { maybeModelData =>
      maybeModelData.map { postData =>
        createModelResource(postData)
      }
    }
  }

  def find(implicit mc: MarkerContext): Future[Iterable[ModelResource]] = {
    postRepository.list().map { postDataList =>
      postDataList.map(postData => createModelResource(postData))
    }
  }

  private def createModelResource(p: ModelData): ModelResource = {
    ModelResource(p.id.toString, routerProvider.get.link(p.id), p.title, p.body)
  }

}
