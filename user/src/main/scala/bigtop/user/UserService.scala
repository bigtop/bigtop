package bigtop
package user

import akka.dispatch.{Future, Promise}
import akka.util.Timeout
import akka.util.duration._
import blueeyes.BlueEyesServiceBuilder
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http.{HttpRequest, HttpResponse, HttpStatus}
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.http.combinators.HttpRequestCombinators
import blueeyes.core.data.{ByteChunk, Bijection, BijectionsChunkJson, BijectionsChunkFutureJson}
import blueeyes.core.service.{ServerHealthMonitorService, HttpServiceHandler,  ServiceContext, HttpService}
import blueeyes.json.JsonAST.{JNothing, JValue}
import blueeyes.json.JsonParser._
import blueeyes.json.Validation._
import java.net.URLDecoder._
import net.lag.logging.Logger
import scalaz.{NonEmptyList, Scalaz, Validation, Success, Failure}
import scalaz.syntax.validation._
import scalaz.std.option.optionSyntax._
import scalaz.std.option._
import bigtop.json.JsonImplicits
import bigtop.concurrent.{FutureValidation, FutureImplicits}


trait UserService[Config,U <: User]
     extends BlueEyesServiceBuilder
     with HttpRequestCombinators
     with BijectionsChunkJson
     with BijectionsChunkFutureJson
     with UserTypes[U]
{
  import FutureImplicits._
  import ErrorImplicits._

  def initialize(context: ServiceContext): Future[Config]
  def makeActions(config: Config): UserActions[U]
  implicit def writer: Writer[U]

  implicit def identityWriter: Writer[JValue] = new Writer[JValue] {
    def write(j: JValue) = j
  }
  implicit def defaultTimeout = Timeout(3.seconds)

  def getUser(req: HttpRequest[Future[JValue]]) =
    req.parameters.get('user).toSuccess[Error](ErrorCode.NoUserGiven).toValidationNel.fv

  /** Get content as JSON and transform to future validation */
  def getContent(request: HttpRequest[Future[JValue]]): JsonValidation =
    request.content.fold(
      some = _.map(_.success[Error].toValidationNel).fv,
      none = ErrorCode.NoContent.fail[JValue].toValidationNel.fv
    )

  def respond[T](f: HttpServiceHandler[Future[JValue], FutureValidation[NonEmptyList[Error],T]])
                (implicit w: Writer[T]):
                  HttpService[Future[JValue], Future[HttpResponse[JValue]]] = {
    (req: HttpRequest[Future[JValue]]) => f(req).fold (
      failure = _.toResponse[JValue],
      success = v => HttpResponse[JValue](content = Some(w.write(v)))
    )
  }

  val userService =
    service("user", "1.0.0") {
      requestLogging(defaultTimeout) {
        healthMonitor(defaultTimeout) { monitor => context =>
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
            Promise.successful(())
          }
        }
      }
    }

}
