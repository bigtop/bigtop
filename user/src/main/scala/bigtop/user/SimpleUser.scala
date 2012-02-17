package bigtop
package user

import bigtop.problem.Problem
import bigtop.problem.Problems._
import bigtop.json._
import bigtop.util.BCrypt
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

case class SimpleUser(val username: String, val password: Password) extends User {

  def isPasswordOk(passwd: String) =
    BCrypt.compare(passwd, password.hash)

}

trait SimpleUserExternalWriter extends JsonWriter[SimpleUser] {
  def write(user: SimpleUser) =
    ("typename" -> "simpleuser") ~
    ("username" -> user.username)
}

trait SimpleUserInternalWriter extends JsonWriter[SimpleUser] {
  def write(user: SimpleUser) =
    ("typename" -> "simpleuser") ~
    ("username" -> user.username) ~
    ("password" -> user.password.hash)
}

trait SimpleUserExternalReader extends JsonReader[Problem,SimpleUser] {
  import JsonImplicits._

  def read(data: JValue) =
    for {
      username <- data.mandatory[String]("username")
      password <- data.mandatory[String]("password")
    } yield SimpleUser(username, Password.fromPassword(password))
}

trait SimpleUserInternalReader extends JsonReader[Problem,SimpleUser] {
  import JsonImplicits._

  def read(data: JValue) =
    for {
      username <- data.mandatory[String]("username")
      password <- data.mandatory[String]("password")
    } yield SimpleUser(username, Password.fromHash(password))
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
