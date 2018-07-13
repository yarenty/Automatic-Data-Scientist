package com.yarenty.ml.api

import cats.effect.IO
import cats.syntax.semigroupk._
import fs2.StreamApp.ExitCode
import fs2.{Stream, StreamApp}
import org.http4s.HttpService
import org.http4s.rho.swagger.syntax.{io => ioSwagger}
import org.http4s.rho.swagger.syntax.io._
import org.http4s.server.blaze.BlazeBuilder
import org.log4s.getLogger
import scala.concurrent.ExecutionContext.Implicits.global



object Server   extends StreamApp[IO] {
  private val logger = getLogger 

  
  
    val port: Int = Option(System.getenv("HTTP_PORT"))
      .map(_.toInt)
      .getOrElse(8080)

    logger.info(s"Starting Swagger example on '$port'")

    def stream(args: List[String], requestShutdown: IO[Unit]): Stream[IO, ExitCode] = {
      val middleware = createRhoMiddleware()

      val mlService: HttpService[IO] = new MLaaSService[IO](ioSwagger) {}.toService(middleware)

      def adsService: HttpService[IO] = new ADSService[IO](ioSwagger) {}.toService(middleware)
      def dsService: HttpService[IO] = new DatasetService[IO](ioSwagger) {}.toService(middleware)

      BlazeBuilder[IO]
        .mountService(StaticContentService.routes)
        .mountService(mlService)
        .bindLocal(port)
        .serve
    }
  }