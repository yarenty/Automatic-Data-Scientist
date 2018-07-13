package com.yarenty.ml.api

import cats.effect.IO
import org.http4s.{HttpService, Request, Response, StaticFile}
import org.http4s.dsl.io._

object StaticContentService {
  private val swaggerUiDir = "/swagger-ui"

  def fetchResource(path: String, req: Request[IO]): IO[Response[IO]] = {
    StaticFile.fromResource(path, Some(req)).getOrElseF(NotFound())
  }

  /**
    * Routes for getting static resources. These might be served more efficiently by apache2 or nginx,
    * but its nice to keep it self contained
    */
  def routes: HttpService[IO] = HttpService {
    // Swagger User Interface
    case req @ GET -> Root / "css" / _       => fetchResource(swaggerUiDir + req.pathInfo, req)
    case req @ GET -> Root / "images" / _    => fetchResource(swaggerUiDir + req.pathInfo, req)
    case req @ GET -> Root / "lib" / _       => fetchResource(swaggerUiDir + req.pathInfo, req)
    case req @ GET -> Root / "swagger-ui"    => fetchResource(swaggerUiDir + "/index.html", req)
    case req @ GET -> Root / "swagger-ui.js" => fetchResource(swaggerUiDir + "/swagger-ui.js", req)
    case req @ GET -> Root / "swagger-ui-bundle.js" => fetchResource(swaggerUiDir + "/swagger-ui-bundle.js", req)
    case req @ GET -> Root / "swagger-ui-standalone-preset.js" => fetchResource(swaggerUiDir + "/swagger-ui-standalone-preset.js", req)
    case req @ GET -> Root / "swagger-ui.css" => fetchResource(swaggerUiDir + "/swagger-ui.css", req)
    case req @ GET -> Root / "index.js" => fetchResource(swaggerUiDir + "/index.js", req)
    case req @ GET -> Root / "absolute-path.js" => fetchResource(swaggerUiDir + "/absolute-path.js", req)

//
//
//      index.html                          swagger-ui-bundle.js                swagger-ui.css
//      absolute-path.js                    index.js                            swagger-ui-bundle.js.map            swagger-ui.css.map
//    favicon-16x16.png                   oauth2-redirect.html                swagger-ui-standalone-preset.js     swagger-ui.js
//    favicon-32x32.png                   package.json                        swagger-ui-standalone-preset.js.map swagger-ui.js.map
      
      
      
  }
}
