package com.yarenty.ml.api

import cats.effect.Effect
import io.circe.Json
import org.http4s.HttpService
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl

class ADSService[F[_]: Effect] extends Http4sDsl[F] {

  val service: HttpService[F] = {
    HttpService[F] {
      case GET -> Root => //list
        Ok(Json.obj("message" -> Json.fromString(s"List of ADSes working")))
      case GET -> Root / id => //get
        Ok(Json.obj("message" -> Json.fromString(s"Get,  ADS id ${id}")))
      case PUT -> Root => //create
        Ok(Json.obj("message" -> Json.fromString(s"Create,  PUT")))
      case DELETE -> Root / id => //cancel
        Ok(Json.obj("message" -> Json.fromString(s"Cancel,  ${id}")))
      case GET -> Root / "status" => Ok("READY")
    }
  }
}