package bigtop
package user

import bigtop.problem.Problem
import bigtop.problem.Problems._
import bigtop.json._
import bigtop.util._
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import scalaz.Validation
import scalaz.syntax.validation._

case class Password private(val hash: String)

object Password {

  def fromHash(hash: String) =
    new Password(hash)

  def fromPassword(password: String) =
    new Password(BCrypt.hash(password))
}

case class SimpleUser(
  val id: Uuid,
  val username: String,
  val password: Password,
  val forenames: String,
  val surname: String,
  admin: Boolean
) extends User {
  lazy val forename =
    forenames.split("[ \t]+").toList.headOption.getOrElse("")

  def name =
    forename + " " + surname

  def isPasswordOk(passwd: String) =
    BCrypt.compare(passwd, password.hash)
}

trait SimpleUserExternalWriter extends JsonWriter[SimpleUser] with JsonFormatters {
  def write(user: SimpleUser) =
    ("typename"  -> "simpleuser") ~
    ("id"        -> user.id.toJson) ~
    ("username"  -> user.username) ~
    ("forenames" -> user.forenames) ~
    ("surname"   -> user.surname)
}

trait SimpleUserInternalWriter extends JsonWriter[SimpleUser] with JsonFormatters {
  def write(user: SimpleUser) =
    ("typename " -> "simpleuser") ~
    ("id"        -> user.id.toJson) ~
    ("username"  -> user.username) ~
    ("forenames" -> user.forenames) ~
    ("surname"   -> user.surname) ~
    ("password"  -> user.password.hash) ~
    ("admin"     -> user.admin)
}

trait SimpleUserExternalReader extends JsonReader[Problem,SimpleUser] {
  import JsonImplicits._

  def read(data: JValue) =
    for {
      username  <- data.mandatory[String]("username")
      password  <- data.mandatory[String]("password")
      forenames <- data.mandatory[String]("forenames")
      surname   <- data.mandatory[String]("surname")
      admin     <- data.mandatory[Boolean]("admin")
    } yield SimpleUser(Uuid.create, username, Password.fromPassword(password), forenames, surname, admin)
}

trait SimpleUserInternalReader extends JsonReader[Problem,SimpleUser] {
  import JsonImplicits._

  def read(data: JValue) =
    for {
      id        <- data.mandatory[Uuid]("id")
      username  <- data.mandatory[String]("username")
      password  <- data.mandatory[String]("password")
      forenames <- data.mandatory[String]("forenames")
      surname   <- data.mandatory[String]("surname")
      admin     <- data.mandatory[Boolean]("admin")
    } yield SimpleUser(id, username, Password.fromHash(password), forenames, surname, admin)
}

trait SimpleUserExternalFormat extends JsonFormat[Problem,SimpleUser]
     with JsonUpdater[Problem,SimpleUser]
     with SimpleUserExternalWriter
     with SimpleUserExternalReader
{
  def update(user: SimpleUser, data: JValue) =
    // Not implemented. Just a hack to get it to compile.
    Client.notImplemented("SimpleUserExternalFormat.update").fail
}

trait SimpleUserExternalImplicits {

  implicit val format = new SimpleUserExternalFormat {}

}

object SimpleUserExternalImplicits extends SimpleUserExternalImplicits
