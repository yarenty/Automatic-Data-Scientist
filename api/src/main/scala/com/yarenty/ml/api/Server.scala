package com.yarenty.ml.api

import cats.effect.IO
import cats.syntax.semigroupk._
import fs2.StreamApp.ExitCode
import fs2.{Stream, StreamApp}
import org.apache.spark.SparkContext
import org.apache.spark.h2o.H2OContext
import org.apache.spark.sql.SparkSession
import org.http4s.HttpService
import org.http4s.rho.swagger.syntax.{io => ioSwagger}
import org.http4s.rho.swagger.syntax.io._
import org.http4s.server.blaze.BlazeBuilder
import org.log4s.getLogger
import water.support.{SparkContextSupport, SparklingWaterApp}

import scala.concurrent.ExecutionContext.Implicits.global



object Server extends StreamApp[IO] with SparklingWaterApp with SparkContextSupport {

  implicit val sc = new SparkContext(configure("MLaaS"))
  implicit val sqlContext = SparkSession.builder().getOrCreate().sqlContext
  sqlContext.sql("SET spark.sql.autoBroadcastJoinThreshold=-1")
  implicit val h2oContext = H2OContext.getOrCreate(sc)
  
  private val logger = getLogger 

  
  
    val port: Int = Option(System.getenv("HTTP_PORT"))
      .map(_.toInt)
      .getOrElse(8080)

    logger.info(s"Starting Swagger example on '$port'")

    def stream(args: List[String], requestShutdown: IO[Unit]): Stream[IO, ExitCode] = {
      val middleware = createRhoMiddleware()

      val mlService: HttpService[IO] = new MLaaSService[IO](ioSwagger) {}.toService(middleware)
      
      BlazeBuilder[IO]
        .mountService(StaticContentService.routes)
        .mountService(mlService)
        .withBanner(List("Machine Learning as a Service"))
        .bindLocal(port)
        .serve
    }
  }