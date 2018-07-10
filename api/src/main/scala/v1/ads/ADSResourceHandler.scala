package v1.ads

import javax.inject.{Inject, Provider}

import play.api.MarkerContext

import scala.concurrent.{ExecutionContext, Future}
import play.api.libs.json._

/**
  * DTO for displaying post information.
  */
case class ADSResource(id: String, link: String, title: String, body: String)

object ADSResource {

  /**
    * Mapping to write a ADSResource out as a JSON value.
    */
  implicit val implicitWrites = new Writes[ADSResource] {
    def writes(post: ADSResource): JsValue = {
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
  * Controls access to the backend data, returning [[ADSResource]]
  */
class ADSResourceHandler @Inject()(
    routerProvider: Provider[ADSRouter],
    postRepository: ADSRepository)(implicit ec: ExecutionContext) {

  def create(postInput: ADSFormInput)(implicit mc: MarkerContext): Future[ADSResource] = {
    val data = ADSData(ADSId("999"), postInput.title, postInput.body)
    // We don't actually create the post, so return what we have
    postRepository.create(data).map { id =>
      createADSResource(data)
    }
  }

  def lookup(id: String)(implicit mc: MarkerContext): Future[Option[ADSResource]] = {
    val postFuture = postRepository.get(ADSId(id))
    postFuture.map { maybeADSData =>
      maybeADSData.map { postData =>
        createADSResource(postData)
      }
    }
  }

  def find(implicit mc: MarkerContext): Future[Iterable[ADSResource]] = {
    postRepository.list().map { postDataList =>
      postDataList.map(postData => createADSResource(postData))
    }
  }

  private def createADSResource(p: ADSData): ADSResource = {
    ADSResource(p.id.toString, routerProvider.get.link(p.id), p.title, p.body)
  }

}
