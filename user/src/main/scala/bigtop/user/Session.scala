package bigtop
package user

import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import bigtop.concurrent.FutureValidation
import bigtop.json._
import bigtop.json.JsonFormatters._
import bigtop.problem._
import bigtop.util.Uuid
import scala.collection.mutable
import scalaz._
import scalaz.Scalaz._

case class Session[U <: User](
  val id: Uuid,
  val realUser: U,
  val effectiveUser: U,
  val session: mutable.Map[String, JValue]
)

object Session {
  trait SessionFormat[U <: User] extends JsonFormat[Session[U]] {
    implicit def userFormat: JsonFormat[U]

    def write(in: Session[U]): JValue = {
      ("typename" -> "session") ~
      ("id"       -> in.id.toJson) ~
      ("session"  -> in.session.toList.map(pair => JField(pair._1, pair._2))) ~
      ("user"     -> in.effectiveUser.toJson) ~
      ("realUser" -> (if(in.realUser.id == in.effectiveUser.id) JNothing else in.realUser.toJson))
    }

    def read(in: JValue): Validation[JsonErrors, Session[U]] = {
      for {
        (id, session, effectiveUser) <- tuple(
                                          in.mandatory[Uuid]("id"),
                                          in.mandatoryMap[JValue]("session").map { map =>
                                            mutable.HashMap(map.toList : _*)
                                          },
                                          in.mandatory[U]("user")
                                        )
        realUser                     <- in.optional[U]("realUser").map(_.getOrElse(effectiveUser))
      } yield Session(
        id             = id,
        realUser       = realUser,
        effectiveUser  = effectiveUser,
        session        = session
      )
    }
  }

  def internalFormat[U <: User](implicit uFormat: JsonFormat[U]) = {
    new SessionFormat[U] {
      implicit val userFormat = uFormat
    }
  }

  def externalFormat[U <: User](implicit uFormat: JsonFormat[U]) = {
    new SessionFormat[U] {
      implicit val userFormat = uFormat
    }
  }
}
