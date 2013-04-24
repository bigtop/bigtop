package bigtop
package user

import akka.dispatch.Future
import blueeyes.core.service.{AsyncHttpService, HttpService, HttpServiceHandler, HttpRequestHandlerCombinators}
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http.{HttpRequest, HttpResponse, HttpStatus}
import blueeyes.core.data.{ByteChunk, Bijection}
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import bigtop.http.{JsonSyncService, SafeBijectionsChunkJson, SafeBijectionsChunkFutureJson}
import bigtop.json.{JsonWriter, JsonFormatters}
import bigtop.concurrent.{FutureImplicits, FutureValidation}
import bigtop.problem.{Problem, Problems}
import bigtop.util.Uuid
import com.weiglewilczek.slf4s.Logging
import scalaz.{Validation, Success, Failure}
import scalaz.syntax.validation._
import scalaz.std.option.optionSyntax._
import scalaz.std.option._


object UserServiceHandler extends SafeBijectionsChunkJson
    with SafeBijectionsChunkFutureJson
    with FutureImplicits
    with HttpRequestHandlerCombinators
    with JsonFormatters
    with Logging
{

  def apply[U <: User](sessionServices: SessionServices[U], userServices: UserServices[U]): AsyncHttpService[ByteChunk] = {
    implicit val log = logger

    JsonSyncService(
      name      = "Session",
      prefix    = "/api/session/v1",
      uuidParam = 'id,
      create    = sessionServices.create,
      read      = sessionServices.read
    ) ~
    JsonSyncService(
      name      = "User",
      prefix    = "/api/user/v1",
      uuidParam = 'id,
      create    = userServices.create,
      read      = userServices.read,
      update    = userServices.update,
      delete    = userServices.delete
    )
  }
}
