package v1.post

import javax.inject.{Inject, Provider}

import play.api.MarkerContext

import scala.concurrent.{ExecutionContext, Future}
import play.api.libs.json._

/**
  * DTO for displaying post information.
  */
case class DatasetResource(id: String, link: String, title: String, body: String)

object DatasetResource {

  /**
    * Mapping to write a DatasetResource out as a JSON value.
    */
  implicit val implicitWrites = new Writes[DatasetResource] {
    def writes(post: DatasetResource): JsValue = {
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
  * Controls access to the backend data, returning [[DatasetResource]]
  */
class DatasetResourceHandler @Inject()(
    routerProvider: Provider[DatasetRouter],
    postRepository: DatasetRepository)(implicit ec: ExecutionContext) {

  def create(postInput: DatasetFormInput)(implicit mc: MarkerContext): Future[DatasetResource] = {
    val data = DatasetData(DatasetId("999"), postInput.title, postInput.body)
    // We don't actually create the post, so return what we have
    postRepository.create(data).map { id =>
      createDatasetResource(data)
    }
  }

  def lookup(id: String)(implicit mc: MarkerContext): Future[Option[DatasetResource]] = {
    val postFuture = postRepository.get(DatasetId(id))
    postFuture.map { maybeDatasetData =>
      maybeDatasetData.map { postData =>
        createDatasetResource(postData)
      }
    }
  }

  def find(implicit mc: MarkerContext): Future[Iterable[DatasetResource]] = {
    postRepository.list().map { postDataList =>
      postDataList.map(postData => createDatasetResource(postData))
    }
  }

  private def createDatasetResource(p: DatasetData): DatasetResource = {
    DatasetResource(p.id.toString, routerProvider.get.link(p.id), p.title, p.body)
  }

}
