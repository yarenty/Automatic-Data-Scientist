package v1.service.prediction

import javax.inject.{Inject, Singleton}

import akka.actor.ActorSystem
import play.api.libs.concurrent.CustomExecutionContext
import play.api.{Logger, MarkerContext}

import scala.concurrent.Future

final case class PredictionServiceData(id: PredictionServiceId, title: String, body: String)

class PredictionServiceId private (val underlying: Int) extends AnyVal {
  override def toString: String = underlying.toString
}

object PredictionServiceId {
  def apply(raw: String): PredictionServiceId = {
    require(raw != null)
    new PredictionServiceId(Integer.parseInt(raw))
  }
}


class PredictionServiceExecutionContext @Inject()(actorSystem: ActorSystem) extends CustomExecutionContext(actorSystem, "repository.dispatcher")

/**
  * A pure non-blocking interface for the PredictionServiceRepository.
  */
trait PredictionServiceRepository {
  def create(data: PredictionServiceData)(implicit mc: MarkerContext): Future[PredictionServiceId]

  def list()(implicit mc: MarkerContext): Future[Iterable[PredictionServiceData]]

  def get(id: PredictionServiceId)(implicit mc: MarkerContext): Future[Option[PredictionServiceData]]
}

/**
  * A trivial implementation for the PredictionService Repository.
  *
  * A custom execution context is used here to establish that blocking operations should be
  * executed in a different thread than Play's ExecutionContext, which is used for CPU bound tasks
  * such as rendering.
  */
@Singleton
class PredictionServiceRepositoryImpl @Inject()()(implicit ec: PredictionServiceExecutionContext) extends PredictionServiceRepository {

  private val logger = Logger(this.getClass)

  private val postList = List(
    PredictionServiceData(PredictionServiceId("1"), "title 1", "blog post 1"),
    PredictionServiceData(PredictionServiceId("2"), "title 2", "blog post 2"),
    PredictionServiceData(PredictionServiceId("3"), "title 3", "blog post 3"),
    PredictionServiceData(PredictionServiceId("4"), "title 4", "blog post 4"),
    PredictionServiceData(PredictionServiceId("5"), "title 5", "blog post 5")
  )

  override def list()(implicit mc: MarkerContext): Future[Iterable[PredictionServiceData]] = {
    Future {
      logger.trace(s"list: ")
      postList
    }
  }

  override def get(id: PredictionServiceId)(implicit mc: MarkerContext): Future[Option[PredictionServiceData]] = {
    Future {
      logger.trace(s"get: id = $id")
      postList.find(post => post.id == id)
    }
  }

  def create(data: PredictionServiceData)(implicit mc: MarkerContext): Future[PredictionServiceId] = {
    Future {
      logger.trace(s"create: data = $data")
      data.id
    }
  }

}
