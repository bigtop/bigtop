package bigtop
package user

import akka.actor._
import blueeyes.BlueEyesServiceBuilder
import blueeyes.concurrent.Future
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http.{HttpRequest, HttpResponse, HttpStatus}
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.http.combinators.HttpRequestCombinators
import blueeyes.core.data.{ByteChunk, Bijection, BijectionsChunkJson, BijectionsChunkString, BijectionsIdentity}
import blueeyes.core.service.{ServerHealthMonitorService, HttpRequestHandler, HttpRequestHandler2, HttpServiceContext}
import blueeyes.json.JsonAST.{JNothing, JValue}
import blueeyes.json.JsonParser._
import blueeyes.json.Validation._
import java.net.URLDecoder._
import net.lag.logging.Logger
import scalaz.{NonEmptyList, Scalaz, Validation, Success, Failure}
import Scalaz._
import bigtop.json.{Implicits=>JsonImplicits}
import bigtop.concurrent.{FutureValidation, Implicits=>FutureImplicits}


trait UserService[Config,U <: User]
     extends BlueEyesServiceBuilder
     with HttpRequestCombinators
     with BijectionsChunkJson
     with BijectionsChunkString
{
  import FutureImplicits._
  import ErrorImplicits._

  def initialize(context: HttpServiceContext): Future[Config]
  def makeActions(config: Config): UserActions[U]
  implicit def writer: Writer[U]
  implicit def identityWriter: Writer[JValue] = new Writer[JValue] {
    def write(j: JValue) = j
  }

  def getUser(req: HttpRequest[JValue]) =
    req.parameters.get('user).toSuccess[Error](ErrorCode.NoUserGiven).liftFailNel.fv

  /** Get content as JSON and transform to future validation */
  def getContent(request: HttpRequest[JValue]) =
    request.content.toSuccess[Error](ErrorCode.NoContent).liftFailNel.fv

  def respond[T](f: HttpRequest[JValue] => FutureValidation[NonEmptyList[Error],T])
                (implicit w: Writer[T]): 
                  HttpRequestHandler[JValue] = {
    case (req: HttpRequest[JValue]) => f(req).fold (
      failure = _.toResponse[JValue],
      success = v => HttpResponse[JValue](content = Some(w.write(v)))
    )
  }

  val userService =
    service("user", "1.0.0") {
      requestLogging {
        logging { implicit logger =>
          healthMonitor { monitor => context =>
            startup {
              initialize(context)
            } ->
            request { config: Config =>
              import JsonImplicits._
              val actions = makeActions(config)
                     
              path("/v1") {
                path("/user") {
                  path("/new") {
                    jvalue {
                      respond(
                        req =>
                          for { 
                            data <- getContent(req)
                            user <- actions.create(data)
                          } yield user
                      )
                    }
                  } ~
                  path("/(?<user>[^/]*)") {
                    path("/login") {
                      jvalue {
                        respond(
                          req =>
                            for {
                              name <- getUser(req) 
                              json <- getContent(req)
                              pwd  <- json./[Error,String]("password", ErrorCode.NoPassword).fv
                              user <- actions.login(name, pwd)
                            } yield user
                        )
                      }
                    } ~
                    path("/update") {
                      jvalue {
                        respond(
                          req =>
                            for {
                              name <- getUser(req)
                              data <- getContent(req)
                              _    <- actions.update(name, data)
                            } yield JNothing: JValue
                        )
                      }
                    } ~
                    path("/delete") {
                      jvalue {
                        respond(
                          req =>
                            for {
                              name <- getUser(req)
                              _    <- actions.delete(name)
                            } yield JNothing: JValue
                        )
                      }
                    }
                  }
                }
              }
            } ->
            shutdown { config =>
              ().future
            }
          }
        }
      }
    }

}
