package bigtop.util

import akka.dispatch.{Future, Promise}
import akka.util.Timeout
import akka.util.duration._
import bigtop.http.SafeBijectionsChunkJson
import blueeyes.BlueEyesServiceBuilder
import blueeyes.concurrent._
import blueeyes.core.data._
import blueeyes.core.http._
import blueeyes.core.http.MimeTypes._
import blueeyes.core.service._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._


case class NotFoundService(val path: String = "/api/.*") extends HttpRequestHandlerCombinators with BijectionsChunkFutureString with SafeBijectionsChunkJson {
  val emptyChunk = Chunk[Array[Byte]]("<none>".getBytes)

  val service: HttpService[ByteChunk,Future[HttpResponse[ByteChunk]]] = {
    path(path) {
      produce(application/json) {
        req: HttpRequest[ByteChunk] =>
          for {
            content <- chunkToFutureString(Timeout(2 seconds))(req.content.getOrElse(emptyChunk))
          } yield HttpResponse[ByteChunk](
            status = HttpStatusCodes.NotFound,
            content = Some(
              ("type" -> "notFound") ~
              ("method" -> req.method.toString) ~
              ("uri" -> req.uri.path.toString) ~
              ("contentType" -> req.headers.get("Content-Type").getOrElse("<none>")) ~
              ("content" -> content)
            )
          )
      }
    }
  }
}
