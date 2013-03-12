package bigtop
package http

import scala.concurrent.duration._
import blueeyes.core.data.BijectionsChunkFutureJson

/** Bijection that doesn't have a timeout of zero */
trait SafeBijectionsChunkFutureJson {
  val safeJsonTimeout = Timeout(200)
  implicit def futureJValueToChunk = BijectionsChunkFutureJson.futureJValueToChunk(safeJsonTimeout)
  implicit def chunkToFutureJValue = futureJValueToChunk.inverse
}

object SafeBijectionsChunkFutureJson extends SafeBijectionsChunkFutureJson
