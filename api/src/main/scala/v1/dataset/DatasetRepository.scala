package v1.post

import javax.inject.{Inject, Singleton}

import akka.actor.ActorSystem
import play.api.libs.concurrent.CustomExecutionContext
import play.api.{Logger, MarkerContext}

import scala.concurrent.Future

final case class DatasetData(id: DatasetId, title: String, body: String)

class DatasetId private (val underlying: Int) extends AnyVal {
  override def toString: String = underlying.toString
}

object DatasetId {
  def apply(raw: String): DatasetId = {
    require(raw != null)
    new DatasetId(Integer.parseInt(raw))
  }
}


class DatasetExecutionContext @Inject()(actorSystem: ActorSystem) extends CustomExecutionContext(actorSystem, "repository.dispatcher")

/**
  * A pure non-blocking interface for the DatasetRepository.
  */
trait DatasetRepository {
  def create(data: DatasetData)(implicit mc: MarkerContext): Future[DatasetId]

  def list()(implicit mc: MarkerContext): Future[Iterable[DatasetData]]

  def get(id: DatasetId)(implicit mc: MarkerContext): Future[Option[DatasetData]]
}

/**
  * A trivial implementation for the Dataset Repository.
  *
  * A custom execution context is used here to establish that blocking operations should be
  * executed in a different thread than Play's ExecutionContext, which is used for CPU bound tasks
  * such as rendering.
  */
@Singleton
class DatasetRepositoryImpl @Inject()()(implicit ec: DatasetExecutionContext) extends DatasetRepository {

  private val logger = Logger(this.getClass)

  private val postList = List(
    DatasetData(DatasetId("1"), "title 1", "blog post 1"),
    DatasetData(DatasetId("2"), "title 2", "blog post 2"),
    DatasetData(DatasetId("3"), "title 3", "blog post 3"),
    DatasetData(DatasetId("4"), "title 4", "blog post 4"),
    DatasetData(DatasetId("5"), "title 5", "blog post 5")
  )

  override def list()(implicit mc: MarkerContext): Future[Iterable[DatasetData]] = {
    Future {
      logger.trace(s"list: ")
      postList
    }
  }

  override def get(id: DatasetId)(implicit mc: MarkerContext): Future[Option[DatasetData]] = {
    Future {
      logger.trace(s"get: id = $id")
      postList.find(post => post.id == id)
    }
  }

  def create(data: DatasetData)(implicit mc: MarkerContext): Future[DatasetId] = {
    Future {
      logger.trace(s"create: data = $data")
      data.id
    }
  }

}
