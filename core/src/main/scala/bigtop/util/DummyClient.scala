package bigtop
package util

import akka.dispatch.{Future, Promise}
import blueeyes.bkka.AkkaDefaults
import blueeyes.core.http._
import blueeyes.core.data.ByteChunk
import blueeyes.core.service.{HttpClient, HttpService}

class DummyClient(service: HttpService[ByteChunk,Future[HttpResponse[ByteChunk]]]) extends HttpClient[ByteChunk] with AkkaDefaults {
  def apply(request: HttpRequest[ByteChunk]) =
    service.service(request).fold(
      fail = f => Promise.successful(
        HttpResponse[ByteChunk](status = HttpStatus(HttpStatusCodes.NotFound))
      ),
      succ = s => s
    )

  def isDefinedAt(x: HttpRequest[ByteChunk]) = true
}
