package v1.service.anomaly

import javax.inject.{Inject, Singleton}

import akka.actor.ActorSystem
import play.api.libs.concurrent.CustomExecutionContext
import play.api.{Logger, MarkerContext}

import scala.concurrent.Future

final case class AnomalyServiceData(id: AnomalyServiceId, title: String, body: String)

class AnomalyServiceId private (val underlying: Int) extends AnyVal {
  override def toString: String = underlying.toString
}

object AnomalyServiceId {
  def apply(raw: String): AnomalyServiceId = {
    require(raw != null)
    new AnomalyServiceId(Integer.parseInt(raw))
  }
}


class AnomalyServiceExecutionContext @Inject()(actorSystem: ActorSystem) extends CustomExecutionContext(actorSystem, "repository.dispatcher")

/**
  * A pure non-blocking interface for the AnomalyServiceRepository.
  */
trait AnomalyServiceRepository {
  def create(data: AnomalyServiceData)(implicit mc: MarkerContext): Future[AnomalyServiceId]

  def list()(implicit mc: MarkerContext): Future[Iterable[AnomalyServiceData]]

  def get(id: AnomalyServiceId)(implicit mc: MarkerContext): Future[Option[AnomalyServiceData]]
}

/**
  * A trivial implementation for the AnomalyService Repository.
  *
  * A custom execution context is used here to establish that blocking operations should be
  * executed in a different thread than Play's ExecutionContext, which is used for CPU bound tasks
  * such as rendering.
  */
@Singleton
class AnomalyServiceRepositoryImpl @Inject()()(implicit ec: AnomalyServiceExecutionContext) extends AnomalyServiceRepository {

  private val logger = Logger(this.getClass)

  private val postList = List(
    AnomalyServiceData(AnomalyServiceId("1"), "title 1", "blog post 1"),
    AnomalyServiceData(AnomalyServiceId("2"), "title 2", "blog post 2"),
    AnomalyServiceData(AnomalyServiceId("3"), "title 3", "blog post 3"),
    AnomalyServiceData(AnomalyServiceId("4"), "title 4", "blog post 4"),
    AnomalyServiceData(AnomalyServiceId("5"), "title 5", "blog post 5")
  )

  override def list()(implicit mc: MarkerContext): Future[Iterable[AnomalyServiceData]] = {
    Future {
      logger.trace(s"list: ")
      postList
    }
  }

  override def get(id: AnomalyServiceId)(implicit mc: MarkerContext): Future[Option[AnomalyServiceData]] = {
    Future {
      logger.trace(s"get: id = $id")
      postList.find(post => post.id == id)
    }
  }

  def create(data: AnomalyServiceData)(implicit mc: MarkerContext): Future[AnomalyServiceId] = {
    Future {
      logger.trace(s"create: data = $data")
      data.id
    }
  }

}
