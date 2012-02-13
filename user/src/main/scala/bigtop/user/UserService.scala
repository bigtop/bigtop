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
import bigtop.json._
import bigtop.concurrent.{FutureValidation, FutureImplicits}
import bigtop.problem.{Problem, ProblemWriters}
import bigtop.problem.Problems._


trait UserService[U <: User]
     extends BlueEyesServiceBuilder
     with HttpRequestCombinators
     with BijectionsChunkJson
     with BijectionsChunkFutureJson
     with UserTypes[U]
     with ProblemWriters {

  self: UserActionsFactory[U] =>

  import FutureImplicits._
  import JsonFormatters._

  implicit def defaultTimeout = Timeout(3 seconds)

  def getUser(req: HttpRequest[Future[JValue]]) =
    req.parameters.get('user).toSuccess[Problem[String]](Request.NoUser).fv

  /** Get content as JSON and transform to future validation */
  def getContent(request: HttpRequest[Future[JValue]]): JsonValidation =
    request.content.fold(
      some = _.map(_.success[Problem[String]]).fv,
      none = (Request.NoContent : Problem[String]).fail[JValue].fv
    )

  def respond[T](f: HttpServiceHandler[Future[JValue], FutureValidation[Problem[String],T]])
                (implicit w: JsonWriter[T]):
                  HttpService[Future[JValue], Future[HttpResponse[JValue]]] = {
    (req: HttpRequest[Future[JValue]]) => f(req).fold (
      failure = _.toResponse[JValue],
      success = v => HttpResponse[JValue](content = Some(w.write(v)))
    )
  }

  /** The user actions being used by this service instance. Useful for testing. */
  val userActions: Promise[UserActions[U]] = Promise()

  val userService =
    service("user", "1.0.0") {
      requestLogging(defaultTimeout) {
        healthMonitor(defaultTimeout) { monitor => context =>
          startup {
            setup(context)
          } ->
          request { config: Config =>
            import JsonImplicits._
            val actions = create(config)
            userActions.success(actions)

            path("/user") {
              path("/v1") {
                path("/new") {
                  jvalue {
                    respond(
                      req =>
                        for {
                          data <- getContent(req)
                          user <- actions.create(data)
                        } yield actions.formatter.write(user)
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
                            pwd  <- json./[Problem[String],String]("password", Request.NoPassword).fv
                            user <- actions.login(name, pwd)
                          } yield actions.formatter.write(user)
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
