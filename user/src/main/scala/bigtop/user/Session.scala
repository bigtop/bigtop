package bigtop
package user

import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
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
  trait SessionFormat[U <: User] extends JsonFormat[Problem, Session[U]] {
    implicit def userFormat: JsonFormat[Problem, U]

    def write(in: Session[U]): JValue = {
      ("typename" -> "session") ~
      ("id"       -> in.id.toJson) ~
      ("session"  -> in.session.toList.map(pair => JField(pair._1, pair._2))) ~
      ("user"     -> in.effectiveUser.toJson) ~
      ("realUser" -> (if(in.realUser.id == in.effectiveUser.id) JNothing else in.realUser.toJson))
    }

    def read(in: JValue): Validation[Problem, Session[U]] = {
      for {
        id            <- in.mandatory[Uuid]("id")
        session       <- in.mandatoryMap[JValue]("session").map { map =>
                           mutable.HashMap(map.toList : _*)
                         }
        effectiveUser <- in.mandatory[U]("user")
        realUser      <- in.optional[U]("realUser").map(_.getOrElse(effectiveUser))
      } yield Session(
        id             = id,
        realUser       = realUser,
        effectiveUser  = effectiveUser,
        session        = session
      )
    }
  }

  def internalFormat[U <: User](implicit uFormat: JsonFormat[Problem, U], uManifest: Manifest[U]) = {
    new SessionFormat[U] {
      implicit val userFormat = uFormat
      implicit val valueManifest: Manifest[Session[U]] = manifest[Session[U]]
    }
  }

  def externalFormat[U <: User](implicit uFormat: JsonFormat[Problem, U], uManifest: Manifest[U]) = {
    new SessionFormat[U] {
      implicit val userFormat = uFormat
      implicit val valueManifest: Manifest[Session[U]] = manifest[Session[U]]
    }
  }
}
