package bigtop
package user

import bigtop.json._
import bigtop.util._
import bigtop.problem._
import bigtop.concurrent._
import bigtop.concurrent.FutureImplicits._
import com.redis._
import com.redis.serialization.{
  Format => RedisFormat,
  Parse  => RedisParse
}
import com.weiglewilczek.slf4s.Logging
import scalaz.syntax.validation._
import scalaz.std.option.optionSyntax._

trait RedisSessionActions[U <: User] extends SessionActions[U] {

  def userActions: UserActions[U]
  def sessionFormat: JsonFormat[Problem, Session[U]]

  def autoLogoutMillis: Option[Int] = None

  def keyPrefix: String = "session-"

  def redisPool: RedisClientPool = new RedisClientPool("localhost", 6379)

  def read(id: Uuid): FutureValidation[Session[U]] = {
    cache.get(id.toString).toSuccess(Problems.NoSession).fv
  }

  def save(session: Session[U]): FutureValidation[Session[U]] = {
    cache.set(session.id.toString, session)
    session.success.fv
  }

  def delete(id: Uuid): FutureValidation[Unit] = {
    cache.delete(id.toString)
    ().success.fv
  }

  object cache extends SimpleRedisCache[Session[U]] {

    val pool = redisPool
    val prefix = keyPrefix
    val ttl = autoLogoutMillis

    def create(session: Session[U]): String = {
      val sessionId = Uuid.create.toString
      set(sessionId, session)
      sessionId
    }

    implicit object valueFormat extends RedisFormat({
      case session: Session[U] =>
        import blueeyes.json.Printer._
        compact(render(sessionFormat.write(session))).getBytes("UTF-8")
    })

    implicit object valueParse extends RedisParse[Session[U]]({ data: Array[Byte] =>
      import blueeyes.json.JsonParser

      val session =
        for {
          json      <- JsonParser.parseValidated(new String(data, "UTF-8"))
          session   <- sessionFormat.read(json)
        } yield session

      session.fold(
        fail = f => sys.error(f.toString),
        succ = s => s
      )
    })
  }
}
