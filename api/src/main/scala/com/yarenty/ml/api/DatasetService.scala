package com.yarenty.ml.api

import cats.effect.Effect
import io.circe.Json
import org.http4s.HttpService
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl

class DatasetService[F[_]: Effect] extends Http4sDsl[F] {

  val service: HttpService[F] = {
    HttpService[F] {
      case GET -> Root => //list
        Ok(Json.obj("datasets" -> Json.fromString(s"List of ADSes working")))
      case GET -> Root / id => //get
        Ok(Json.obj("dataset" -> Json.fromString(s"Get,  ADS id ${id}")))
      case POST -> Root => //create
        Ok(Json.obj("new dataset" -> Json.fromString(s"Create,  PUT")))
      case DELETE -> Root / id => //cancel
        Ok(Json.obj("Cancel" -> Json.fromString(s"Cancel,  ${id}")))
      case PATCH -> Root /id => //create
        Ok(Json.obj("update dataset" -> Json.fromString(s"Update,  PATCH $id")))
      case GET -> Root / "status" => Ok("READY")
    }
  }
}