package bigtop
package http

import akka.util.Timeout
import blueeyes.core.data.BijectionsChunkFutureJson

/** Bijection that doesn't have a timeout of zero */
trait SafeBijectionsChunkFutureJson {
  val jsonTimeout = Timeout(200)
  implicit def futureJValueToChunk = BijectionsChunkFutureJson.futureJValueToChunk(jsonTimeout)
  implicit def chunkToFutureJValue = futureJValueToChunk.inverse
}

object SafeBijectionsChunkFutureJson extends SafeBijectionsChunkFutureJson
