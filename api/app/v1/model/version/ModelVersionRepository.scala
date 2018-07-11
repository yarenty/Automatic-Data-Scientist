package v1.model.version

import javax.inject.{Inject, Singleton}

import akka.actor.ActorSystem
import play.api.libs.concurrent.CustomExecutionContext
import play.api.{Logger, MarkerContext}

import scala.concurrent.Future

final case class ModelVersionData(id: ModelVersionId, title: String, body: String)

class ModelVersionId private (val underlying: Int) extends AnyVal {
  override def toString: String = underlying.toString
}

object ModelVersionId {
  def apply(raw: String): ModelVersionId = {
    require(raw != null)
    new ModelVersionId(Integer.parseInt(raw))
  }
}


class ModelVersionExecutionContext @Inject()(actorSystem: ActorSystem) extends CustomExecutionContext(actorSystem, "repository.dispatcher")

/**
  * A pure non-blocking interface for the ModelVersionRepository.
  */
trait ModelVersionRepository {
  def create(data: ModelVersionData)(implicit mc: MarkerContext): Future[ModelVersionId]

  def list()(implicit mc: MarkerContext): Future[Iterable[ModelVersionData]]

  def get(id: ModelVersionId)(implicit mc: MarkerContext): Future[Option[ModelVersionData]]
}

/**
  * A trivial implementation for the ModelVersion Repository.
  *
  * A custom execution context is used here to establish that blocking operations should be
  * executed in a different thread than Play's ExecutionContext, which is used for CPU bound tasks
  * such as rendering.
  */
@Singleton
class ModelVersionRepositoryImpl @Inject()()(implicit ec: ModelVersionExecutionContext) extends ModelVersionRepository {

  private val logger = Logger(this.getClass)

  private val postList = List(
    ModelVersionData(ModelVersionId("1"), "title 1", "blog post 1"),
    ModelVersionData(ModelVersionId("2"), "title 2", "blog post 2"),
    ModelVersionData(ModelVersionId("3"), "title 3", "blog post 3"),
    ModelVersionData(ModelVersionId("4"), "title 4", "blog post 4"),
    ModelVersionData(ModelVersionId("5"), "title 5", "blog post 5")
  )

  override def list()(implicit mc: MarkerContext): Future[Iterable[ModelVersionData]] = {
    Future {
      logger.trace(s"list: ")
      postList
    }
  }

  override def get(id: ModelVersionId)(implicit mc: MarkerContext): Future[Option[ModelVersionData]] = {
    Future {
      logger.trace(s"get: id = $id")
      postList.find(post => post.id == id)
    }
  }

  def create(data: ModelVersionData)(implicit mc: MarkerContext): Future[ModelVersionId] = {
    Future {
      logger.trace(s"create: data = $data")
      data.id
    }
  }

}
