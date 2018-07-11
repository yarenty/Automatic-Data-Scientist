package v1.ads

import javax.inject.{Inject, Singleton}

import akka.actor.ActorSystem
import play.api.libs.concurrent.CustomExecutionContext
import play.api.{Logger, MarkerContext}
import scala.concurrent.Future

final case class ADSData(id: ADSId, title: String, body: String)

class ADSId private (val underlying: Int) extends AnyVal {
  override def toString: String = underlying.toString
}

object ADSId {
  def apply(raw: String): ADSId = {
    require(raw != null)
    new ADSId(Integer.parseInt(raw))
  }
}


class ADSExecutionContext @Inject()(actorSystem: ActorSystem) extends CustomExecutionContext(actorSystem, "ads.repository.dispatcher")

/**
  * A pure non-blocking interface for the ADSRepository.
  */
trait ADSRepository {
  def create(data: ADSData)(implicit mc: MarkerContext): Future[ADSId]

  def list()(implicit mc: MarkerContext): Future[Iterable[ADSData]]

  def get(id: ADSId)(implicit mc: MarkerContext): Future[Option[ADSData]]
}

/**
  * A trivial implementation for the ADS Repository.
  *
  * A custom execution context is used here to establish that blocking operations should be
  * executed in a different thread than Play's ExecutionContext, which is used for CPU bound tasks
  * such as rendering.
  */
@Singleton
class ADSRepositoryImpl @Inject()()(implicit adsEC: ADSExecutionContext) extends ADSRepository {

  private val logger = Logger(this.getClass)

  private val adsList = List(
    ADSData(ADSId("1"), "title 1", "blog post 1"),
    ADSData(ADSId("2"), "title 2", "blog post 2"),
    ADSData(ADSId("3"), "title 3", "blog post 3"),
    ADSData(ADSId("4"), "title 4", "blog post 4"),
    ADSData(ADSId("5"), "title 5", "blog post 5")
  )

  override def list()(implicit mc: MarkerContext): Future[Iterable[ADSData]] = {
    Future {
      logger.trace(s"list: ")
      adsList
    }
  }

  override def get(id: ADSId)(implicit mc: MarkerContext): Future[Option[ADSData]] = {
    Future {
      logger.trace(s"get: id = $id")
      adsList.find(ads => ads.id == id)
    }
  }

  def create(data: ADSData)(implicit mc: MarkerContext): Future[ADSId] = {
    Future {
      logger.trace(s"create: data = $data")
      data.id
    }
  }

}
