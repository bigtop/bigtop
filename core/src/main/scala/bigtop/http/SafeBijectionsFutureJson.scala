package bigtop
package http

import akka.dispatch.{Await, Future}
import akka.util.{Duration, Timeout}
import blueeyes.json.JsonAST._
import blueeyes.json.Printer._
import blueeyes.json.JsonParser
import blueeyes.core.data.{AggregatedByteChunk, Bijection, ByteChunk, Chunk}


trait SafeBijectionsChunkJson {
  import java.io.{InputStreamReader, ByteArrayInputStream, OutputStreamWriter, ByteArrayOutputStream}

  implicit val JValueToChunk = new Bijection[JValue, ByteChunk] {
    val jsonTimeout = Timeout(400)

    def apply(t: JValue) = {
      val stream = new ByteArrayOutputStream()

      compact(render(t), new OutputStreamWriter(stream, "UTF-8"))

      Chunk(stream.toByteArray)
    }

    def unapply(s: ByteChunk) = {
      val futureJson =
        AggregatedByteChunk(s, None).map(
          chunk =>
            JsonParser.parse(
              new InputStreamReader(new ByteArrayInputStream(chunk.data), "UTF-8")
            )
        )
      Await.result(futureJson, jsonTimeout.duration)
    }
  }

  implicit val ChunkToJValue = JValueToChunk.inverse
}

object SafeBijectionsChunkJson extends SafeBijectionsChunkJson


/** Bijection that doesn't have a timeout of zero */
trait SafeBijectionsChunkFutureJson {

  import SafeBijectionsChunkJson._
  implicit def futureJValueToChunk = new Bijection[Future[JValue], ByteChunk] {
    val jsonTimeout = Timeout(400)

    def apply(t: Future[JValue]) = Await.result(t.map(JValueToChunk(_)), jsonTimeout.duration)

    def unapply(b: ByteChunk) = AggregatedByteChunk(b, None).map(ChunkToJValue(_))
  }

  implicit def chunkToFutureJValue = futureJValueToChunk.inverse

}

object SafeBijectionsChunkFutureJson extends SafeBijectionsChunkFutureJson
