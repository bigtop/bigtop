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
import blueeyes.core.service.{AsyncHttpService, ServerHealthMonitorService, HttpServiceHandler,  ServiceContext, HttpService}
import blueeyes.json.JsonAST.{JNothing, JValue}
import blueeyes.json.JsonParser._
import blueeyes.json.Validation._
import java.net.URLDecoder._
import net.lag.configgy.ConfigMap
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

  import FutureImplicits._
  import JsonFormatters._

  implicit def defaultTimeout = Timeout(3 seconds)

  def getUser(req: HttpRequest[Future[JValue]]) =
    req.parameters.get('user).toSuccess[Problem](Client.NoUser).fv

  /** Get content as JSON and transform to future validation */
  private def getContent[T](request: HttpRequest[Future[T]]): FutureValidation[Problem, T] =
    request.content.fold(
      some = _.map(_.success[Problem]).fv,
      none = (Client.NoContent : Problem).fail[T].fv
    )

  def respond[T](f: HttpServiceHandler[Future[JValue], FutureValidation[Problem,T]])
                (implicit w: JsonWriter[T]):
                  HttpService[Future[JValue], Future[HttpResponse[JValue]]] = {
    (req: HttpRequest[Future[JValue]]) => f(req).fold (
      failure = _.toResponse,
      success = v => HttpResponse[JValue](content = Some(w.write(v)))
    )
  }

  def createUserActions(config: ConfigMap): UserActions[U]

  val userService =
    service("user", "1.0.0") {
      requestLogging(defaultTimeout) {
        healthMonitor(defaultTimeout) { monitor => context =>
          startup {
            Promise.successful(createUserActions(context.config))
          } ->
          request { userActions =>
            userServiceRequestHandler(userActions)
          } ->
          shutdown { config =>
            Promise.successful(())
          }
        }
      }
    }

  def userServiceRequestHandler(userActions: UserActions[U]): AsyncHttpService[ByteChunk] =
    path("/user") {
      path("/v1") {
        path("/new") {
          jvalue {
            respond(
              req =>
                for {
                  data <- getContent(req)
                  user <- userActions.createUser(data)
                } yield userActions.userFormatter.write(user)
            )
          }
        } ~
        path("/(?<user>[^/]*)") {
          // path("/login") {
          //   jvalue {
          //     respond(
          //       req =>
          //         for {
          //           name <- getUser(req)
          //           json <- getContent(req)
          //           pwd  <- json./[Problem,String]("password", Client.NoPassword).fv
          //           user <- userActions.loginUser(name, pwd)
          //         } yield userActions.userFormatter.write(user)
          //     )
          //   }
          // } ~
          path("/update") {
            jvalue {
              respond(
                req =>
                  for {
                    name <- getUser(req)
                    data <- getContent(req)
                    _    <- userActions.updateUser(name, data)
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
                    _    <- userActions.deleteUser(name)
                  } yield JNothing: JValue
              )
            }
          }
        }
      }
    }

}
