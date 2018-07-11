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
  implicit val implicitADSWrites = new Writes[ADSResource] {
    def writes(ads: ADSResource): JsValue = {
      Json.obj(
        "id" -> ads.id,
        "link" -> ads.link,
        "title" -> ads.title,
        "body" -> ads.body
      )
    }
  }
}

/**
  * Controls access to the backend data, returning [[ADSResource]]
  */
class ADSResourceHandler @Inject()(
    adsRouterProvider: Provider[ADSRouter],
    adsRepository: ADSRepository)(implicit adsEC: ExecutionContext) {

  def create(postInput: ADSFormInput)(implicit mc: MarkerContext): Future[ADSResource] = {
    val data = ADSData(ADSId("999"), postInput.title, postInput.body)
    // We don't actually create the post, so return what we have
    adsRepository.create(data).map { id =>
      createADSResource(data)
    }
  }

  def lookup(id: String)(implicit mc: MarkerContext): Future[Option[ADSResource]] = {
    val postFuture = adsRepository.get(ADSId(id))
    postFuture.map { maybeADSData =>
      maybeADSData.map { adsData =>
        createADSResource(adsData)
      }
    }
  }

  def find(implicit mc: MarkerContext): Future[Iterable[ADSResource]] = {
    adsRepository.list().map { postDataList =>
      postDataList.map(adsData => createADSResource(adsData))
    }
  }

  private def createADSResource(p: ADSData): ADSResource = {
    ADSResource(p.id.toString, adsRouterProvider.get.link(p.id), p.title, p.body)
  }

}
