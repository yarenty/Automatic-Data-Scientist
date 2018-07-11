package v1.model

import javax.inject.{Inject, Singleton}

import akka.actor.ActorSystem
import play.api.libs.concurrent.CustomExecutionContext
import play.api.{Logger, MarkerContext}

import scala.concurrent.Future

final case class ModelData(id: ModelId, title: String, body: String)

class ModelId private (val underlying: Int) extends AnyVal {
  override def toString: String = underlying.toString
}

object ModelId {
  def apply(raw: String): ModelId = {
    require(raw != null)
    new ModelId(Integer.parseInt(raw))
  }
}


class ModelExecutionContext @Inject()(actorSystem: ActorSystem) extends CustomExecutionContext(actorSystem, "repository.dispatcher")

/**
  * A pure non-blocking interface for the ModelRepository.
  */
trait ModelRepository {
  def create(data: ModelData)(implicit mc: MarkerContext): Future[ModelId]

  def list()(implicit mc: MarkerContext): Future[Iterable[ModelData]]

  def get(id: ModelId)(implicit mc: MarkerContext): Future[Option[ModelData]]
}

/**
  * A trivial implementation for the Model Repository.
  *
  * A custom execution context is used here to establish that blocking operations should be
  * executed in a different thread than Play's ExecutionContext, which is used for CPU bound tasks
  * such as rendering.
  */
@Singleton
class ModelRepositoryImpl @Inject()()(implicit ec: ModelExecutionContext) extends ModelRepository {

  private val logger = Logger(this.getClass)

  private val postList = List(
    ModelData(ModelId("1"), "title 1", "blog post 1"),
    ModelData(ModelId("2"), "title 2", "blog post 2"),
    ModelData(ModelId("3"), "title 3", "blog post 3"),
    ModelData(ModelId("4"), "title 4", "blog post 4"),
    ModelData(ModelId("5"), "title 5", "blog post 5")
  )

  override def list()(implicit mc: MarkerContext): Future[Iterable[ModelData]] = {
    Future {
      logger.trace(s"list: ")
      postList
    }
  }

  override def get(id: ModelId)(implicit mc: MarkerContext): Future[Option[ModelData]] = {
    Future {
      logger.trace(s"get: id = $id")
      postList.find(post => post.id == id)
    }
  }

  def create(data: ModelData)(implicit mc: MarkerContext): Future[ModelId] = {
    Future {
      logger.trace(s"create: data = $data")
      data.id
    }
  }

}
