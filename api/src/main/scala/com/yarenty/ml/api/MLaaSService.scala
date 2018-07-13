package com.yarenty.ml.api


import cats.Monad
import cats.effect.Effect
import cats.syntax.either._
import cats.syntax.option._
import com.yarenty.ml.api.JsonEncoder.{AutoSerializable, _}
import com.yarenty.ml.api.MLaaSService._
import com.yarenty.ml.api.repos.ADSMock
import com.yarenty.ml.api.types._
import org.http4s.rho.RhoService
import org.http4s.rho.bits._
import org.http4s.rho.swagger.SwaggerSyntax
import org.http4s.{EntityDecoder, Uri}
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods

import scala.reflect.ClassTag

abstract class MLaaSService[F[+ _] : Effect](swaggerSyntax: SwaggerSyntax[F])(implicit F: Monad[F])
  extends RhoService[F] {

  import swaggerSyntax._


  "We don't want to have a real 'root' route anyway... " **
    GET |>> TemporaryRedirect(Uri(path = "/swagger-ui"))

  // We want to define this chunk of the service as abstract for reuse below
  val status = GET / "status"

  "Status of API" **
    status |>> Ok("MLaaS REST API status: OK !")


  "Get list of ADS processes for the project." **
    GET / "v1" / "ads" |>> {
    Ok(JsonResult("List of ADSes working", 1))
  }

  "The ADS methods performs transformations, trainings, model selection and model reasoning on the data in the request." **
    GET / "v1" / "ads" / pathVar[Int]("id") |>> {
    (id: Int) =>
      println(s"it is there: $id")
      Ok(ADSMock.adsFlow(id))
  }


  "Generates some JSON data from a route param, and a query Int" **
    GET / "v1" / "datasets" |>> {
    Ok(JsonResult("List of datasets", 1))
  }

  "Generates some JSON data from a route param, and a query Int" **
    GET / "v1" / "datasets" / pathVar[Int]("id") |>> { (id: Int) => Ok(JsonResult(" DATA ", id)) }


  "Generates some JSON data from a route param, and a query Int" **
    POST / "v1" / "datasets" ^ EntityDecoder.text[F] |>> { body: String =>
    "You posted: " + body
  }


  "This route allows you to send head request" **
    HEAD / "status" |>> {
    Ok("OK!")
  }


  //
  //  "Generates some JSON data from a route param, and a query Int" **
  //    GET / "result" / 'foo +? param[Int]("id") |>> { (name: String, id: Int) => Ok(JsonResult(name, id)) }
  //
  //  "Two different response codes can result from this route based on the number given" **
  //    GET / "differentstatus" / pathVar[Int] |>> { i: Int =>
  //    if (i >= 0) Ok(JsonResult("Good result", i))
  //    else BadRequest(s"Negative number: $i")
  //  }
  //
  //  "This gets a simple counter for the number of times this route has been requested" **
  //    GET / "counter" |>> {
  //    val i = new AtomicInteger(0)
  //    F.pure(s"The number is ${i.getAndIncrement()}")
  //  }
  //
  //
  //  "This route allows your to post stuff" **
  //    POST / "post" ^ EntityDecoder.text[F] |>> { body: String =>
  //    "You posted: " + body
  //  }
  //
  //  "This demonstrates using a process of entities" **
  //    GET / "stream" |>> {
  //    val s = 0 until 100 map (i => s"Hello $i\n")
  //    val p: Stream[F, String] = Stream.emits(s).covary[F]
  //
  //    Ok(p)
  //  }
  //
  //  "Get a file" **
  //    GET / "file" |>> Ok(SwaggerFileResponse("HELLO"))
  //
  //  "This route demonstrates how to use a complex data type as parameters in route" **
  //    GET / "complex" +? param[Foo]("foo") & param[Seq[Bar]]("bar", Nil) |>> { (foo: Foo, bars: Seq[Bar]) =>
  //    Ok(s"Received foo: $foo, bars: $bars")
  //  }

}

object MLaaSService {

  import scala.reflect.runtime.universe.TypeTag


  case class Foo(k: String, v: Int)

  case class Bar(id: Long, foo: Foo)

  case class JsonResult(name: String, number: Int) extends AutoSerializable

  case class JsonError(error: String) extends AutoSerializable


  private implicit val format: DefaultFormats = DefaultFormats

  implicit def jsonParser[F[_], A: TypeTag : ClassTag]: StringParser[F, A] = new StringParser[F, A] with FailureResponseOps[F] {
    override val typeTag: Option[TypeTag[A]] =
      implicitly[TypeTag[A]].some

    override def parse(s: String)(implicit F: Monad[F]): ResultResponse[F, A] = {

      Either.catchNonFatal(JsonMethods.parse(s).extract[A]) match {
        case Left(t) => badRequest[String](t.getMessage)
        case Right(t) => SuccessResponse(t)
      }
    }
  }
}
