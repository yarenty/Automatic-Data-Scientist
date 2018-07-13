package com.yarenty.ml.api

import java.util.concurrent.atomic.AtomicInteger

import cats.Monad
import cats.effect.Effect
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.option._
import com.yarenty.ml.api.JsonEncoder.{AutoSerializable, _}
import com.yarenty.ml.api.ADSService._
import fs2.Stream
import org.http4s.rho.RhoService
import org.http4s.rho.bits._
import org.http4s.rho.swagger.{SwaggerFileResponse, SwaggerSyntax}
import org.http4s.{EntityDecoder, Headers, HttpDate, Request, Uri, headers}
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods
import shapeless.HNil

import scala.reflect.ClassTag

abstract class ADSService[F[+_] : Effect](swaggerSyntax: SwaggerSyntax[F])(implicit F: Monad[F])
  extends RhoService[F] {

  import swaggerSyntax._


  // We want to define this chunk of the service as abstract for reuse below
//
//  "This route allows you to send head request" **
//    HEAD / "hello" |>> { Ok("Hello ADS!") }

  "Generates some JSON data from a route param, and a query Int" **
    GET |>> { Ok(ADSJsonResult("List of ADSes working")) }
  
//  val service: HttpService[F] = {
//    HttpService[F] {
//      case GET -> root => //list
//        Ok(Json.obj("message" -> Json.fromString(s"List of ADSes working")))
//      case GET -> Root / id => //get
//        Ok(Json.obj("message" -> Json.fromString(s"Get,  ADS id ${id}")))
//      case PUT -> Root => //create
//        Ok(Json.obj("message" -> Json.fromString(s"Create,  PUT")))
//      case DELETE -> Root / id => //cancel
//        Ok(Json.obj("message" -> Json.fromString(s"Cancel,  ${id}")))
//      case GET -> Root / "status" => Ok("READY")
//    }
//  }
}

object ADSService {
  import scala.reflect.runtime.universe.TypeTag

  case class ADSJsonResult(name: String) extends AutoSerializable

  private implicit val format: DefaultFormats = DefaultFormats

  implicit def jsonParser[F[_], A : TypeTag : ClassTag]: StringParser[F, A] = new StringParser[F, A] with FailureResponseOps[F] {
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