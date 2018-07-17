package com.yarenty.ml.api


import cats.Monad
import cats.effect.Effect
import cats.syntax.either._
import cats.syntax.option._
import com.yarenty.ml.api.JsonEncoder.{AutoSerializable, _}
import com.yarenty.ml.api.MLaaSService._
import com.yarenty.ml.api.ads.ADSService
import com.yarenty.ml.api.dataset.DatasetService
import com.yarenty.ml.api.repos.ADSMock
import org.http4s.rho.RhoService
import org.http4s.rho.bits._
import org.http4s.rho.swagger.SwaggerSyntax
import org.http4s.{EntityDecoder, Uri}
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods
import water.fvec.H2OFrame
import water.util.Log

import scala.reflect.ClassTag


/**
  *
  * The [[MLaaSService]] provides a convenient way to define routes in a style
  * similar to scalatra etc by providing implicit conversions.
  *
  * {{{
  *   val srvc = new MLaaSService {
  *     POST / "foo" / pathVar[Int] +? param[String]("param") |>> { (p1: Int, param: String) =>
  *       Ok("success")
  *     }
  *   }
  *
  * }}}
  *
  * C(2018) by yarenty.
  *
  */
abstract class MLaaSService[F[+ _] : Effect](swaggerSyntax: SwaggerSyntax[F])(implicit F: Monad[F])
  extends RhoService[F] {

  import swaggerSyntax._
  
  var data:H2OFrame = null
  
  
  
  "We don't want to have a real 'root' route anyway... " **
    GET |>> TemporaryRedirect(Uri(path = "/swagger-ui"))

  // We want to define this chunk of the service as abstract for reuse below
  val status = GET / "status"

  "Status of API" **
    status |>> Ok("MLaaS REST API status: OK !")

  
  
  

  /*
   Automatic Data Scientist
   */


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

  "Create new ADS flow job." **
    POST / "v1" / "ads" ^ EntityDecoder.text[F] |>> { body: String =>
    
    Log.info("Calculate KPI")
    data = ADSService.calculateKPI(data)
    Log.info("Automatic Feature Engineering: Time Series")
    data = ADSService.calculateTimeKPIS(data)
    
    Log.info("Aomaly Detection ...")
    val ad = ADSService.AnomalyDetection(data)
    
    Ok("You posted: " + body + ad._4.mkString("\n"))
  }

  "Cancel running ADS flow job." **
    DELETE / "v1" / "ads" / pathVar[Int]("id") |>> {
    (id: Int) =>
      println(s"CANCEL: $id")
      Ok(ADSMock.adsFlow(id))
  }

  
  
  

  /*
 Input datasets manipulaitons
 */
  

  "List of  available Datasets" **
    GET / "v1" / "datasets" |>> {
    Ok(JsonDS(DatasetService.getList()))
  }

  "Load dataset." **
    GET / "v1" / "datasets" / 'name |>> { (name: String) =>
    data = DatasetService.loadData(name)
    if (null==data) {
      BadRequest(s"There is no dataset called: [$name] ")
    } else {
      Ok(data.toString() + "\n" + data.toTwoDimTable)
    }
  }


  "Create new dataset" **
    POST / "v1" / "datasets" ^ EntityDecoder.text[F] |>> { body: String =>
    "You posted: " + body
  }

  "DELETE dataset - only if is not used in any existing ADSFlow" **
    DELETE / "v1" / "datasets" / pathVar[Int]("id") |>> {
    (id: Int) =>
      println(s"DELETE dataset: $id")
      Ok("OK")
  }

  case class User(name: String)

  "UPDATE dataset with new data" **
    PATCH / "v1" / "datasets" / pathVar[Int]("id") |>> {
    (id: Int) =>
      println(s"UPDATE dataset: $id")
      Ok("OK")
  }

  "GET slices of dataset - input for ADSFLow (JSON body - configuration!) " **
    POST / "v1" / "datasets" / pathVar[Int]("id") / "getSlices" ^ EntityDecoder.text[F] |>> {
    (id: Int, body: String) =>
      println(s"get sLices of dataset: $id")
      val user: User = User(body)
      Ok(s"OK - here are slices $id: ${user.name}")
  }


  
  
  
  /*
   MODELS
   */

  "List of  available Models" **
    GET / "v1" / "models" |>> {
    Ok(JsonResult("List of models", 1))
  }

  "Get info about model." **
    GET / "v1" / "models" / pathVar[Int]("id") |>> { (id: Int) => Ok(JsonResult(" MODEL ", id)) }


  "Create new model (model is metadata- create version as well)" **
    POST / "v1" / "models" ^ EntityDecoder.text[F] |>> { body: String =>
    "You posted: " + body
  }

  "DELETE model - only if there are all versions deleted" **
    DELETE / "v1" / "models" / pathVar[Int]("id") |>> {
    (id: Int) =>
      println(s"DELETE model: $id")
      Ok("OK")
  }

  "UPDATE model resources" **
    PATCH / "v1" / "models" / pathVar[Int]("id") |>> {
    (id: Int) =>
      println(s"UPDATE model: $id")
      Ok("OK")
  }


  "*EXPERIMENTAL* Get Identity and Access Management Policy for resource." **
    GET / "v1" / "models" / pathVar[Int]("id") / "getIAMPolicy" |>> { (id: Int) => Ok(JsonResult(" Policy for model  ", id)) }

  "*EXPERIMENTAL* Get info about Identity and Access Management Policy." **
    POST / "v1" / "models" / pathVar[Int]("id") / "setIAMPolicy" ^ EntityDecoder.text[F] |>> { (id: Int, body: String) => Ok(JsonResult(" Policy for model  ", id)) }

  "*EXPERIMENTAL* Returns permissions that a caller has on a model." **
    GET / "v1" / "models" / pathVar[Int]("id") / "testIAMPolicy" |>> { (id: Int) => Ok(JsonResult(" Policy for model  ", id)) }


  /*
  VERSIONS of MODELS
  */


  "List of available versions  [for chosen model]" **
    GET / "v1" / "models" / pathVar[Int]("id") / "versions" |>> {
    (id: Int) =>
      Ok(JsonResult("List of versions for model", id))
  }

  "Get particular model/version." **
    GET / "v1" / "models" / pathVar[Int]("id") / "versions" / pathVar[Int]("verId") |>> { (id: Int, verId: Int) => Ok(JsonResult(" MODEL ", verId)) }


  "Create new version of the model (model is metadata- create version as well)" **
    POST / "v1" / "models" / pathVar[Int]("id") / "versions" ^ EntityDecoder.text[F] |>> { (id: Int, body: String) =>
    "You posted: " + body + " for model" + id
  }

  "DELETE version of the model" **
    DELETE / "v1" / "models" / pathVar[Int]("id") / "versions" / pathVar[Int]("verId") |>> {
    (id: Int, verId: Int) =>
      println(s"DELETE model: $id  version: $verId")
      Ok("OK")
  }


  /*
  Prediction service 
  */


  "List of available working prediction services." **
    GET / "v1" / "services" / "predictions" |>> {
    Ok(JsonResult("List of predictions", 1))
  }

  "Get prediction info." **
    GET / "v1" / "services" / "predictions" / pathVar[Int]("id") |>> {
    (id: Int) => Ok(JsonResult(" MODEL ", id))
  }


  "Create new prediction service" **
    POST / "v1" / "services" / "predictions" / pathVar[Int]("id") ^ EntityDecoder.text[F] |>> { (id: Int, body: String) =>
    "You posted: " + body + " for pred" + id
  }

  "STOP prediction service " **
    DELETE / "v1" / "services" / "predictions" / pathVar[Int]("id") |>> {
    (id: Int) =>
      println(s"STOP predictoin: $id  ")
      Ok("OK")
  }

  "Predict!" **
    PUT / "v1" / "services" / "predictions" / pathVar[Int]("id") ^ EntityDecoder.text[F] |>> { (id: Int, body: String) =>
    "You want to predict on: " + body + " for model" + id
  }


  /*
  Anomaly service 
 */


  "List of available working anomaly detection services." **
    GET / "v1" / "services" / "anomalies" |>> {
    Ok(JsonResult("List of predictions", 1))
  }

  "Get anomaly detection service info." **
    GET / "v1" / "services" / "anomalies" / pathVar[Int]("id") |>> {
    (id: Int) => Ok(JsonResult(" MODEL ", id))
  }


  "Create new anomaly detection service" **
    POST / "v1" / "services" / "anomalies" / pathVar[Int]("id") ^ EntityDecoder.text[F] |>> { (id: Int, body: String) =>
    "You posted: " + body + " for AD" + id
  }

  "STOP anomaly detection service " **
    DELETE / "v1" / "services" / "anomalies" / pathVar[Int]("id") |>> {
    (id: Int) =>
      println(s"STOP anomaly detection: $id  ")
      Ok("OK")
  }

  "Predict!" **
    PUT / "v1" / "services" / "anomalies" / pathVar[Int]("id") ^ EntityDecoder.text[F] |>> { (id: Int, body: String) =>
    "You want to detect anomaly  on: " + body + " for model" + id
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

  case class JsonResult(name: String, number: Int) extends AutoSerializable

  case class JsonError(error: String) extends AutoSerializable

  
  // datasets
  case class JsonDS(datasets:List[String]) extends AutoSerializable

  
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
