package bigtop
package user

import akka.dispatch.Future
import blueeyes.core.service.{AsyncHttpService, HttpService, HttpServiceHandler, HttpRequestHandlerCombinators}
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http.{HttpRequest, HttpResponse, HttpStatus}
import blueeyes.core.data.{ByteChunk, Bijection, BijectionsChunkJson, BijectionsChunkFutureJson}
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import bigtop.http.JsonSyncService
import bigtop.json.{JsonImplicits, JsonWriter, JsonFormatters}
import bigtop.concurrent.{FutureImplicits, FutureValidation}
import bigtop.problem.{Problem, Problems, ProblemWriters}
import bigtop.util.Uuid
import net.lag.logging.Logger
import scalaz.{Validation, Success, Failure}
import scalaz.syntax.validation._
import scalaz.std.option.optionSyntax._
import scalaz.std.option._


object UserServiceHandler extends BijectionsChunkJson
    with BijectionsChunkFutureJson
    with FutureImplicits
    with JsonImplicits
    with HttpRequestHandlerCombinators
    with ProblemWriters
    with JsonFormatters
{

  def apply[U <: User](sessionServices: SessionServices[U], userServices: UserServices[U]): AsyncHttpService[ByteChunk] = {
    implicit val log = Logger.get

    JsonSyncService(
      name   = "Session",
      prefix = "/api/session/v1",
      create = sessionServices.create,
      read   = sessionServices.read
    ) ~
    JsonSyncService(
      name = "User",
      prefix = "/api/user/v1",
      create = userServices.create,
      read   = userServices.read,
      update = userServices.update,
      delete = userServices.delete
    )
  }
}
