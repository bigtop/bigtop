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
  val admin: Boolean
) extends User {
  lazy val forename =
    forenames.split("[ \t]+").toList.headOption.getOrElse("")

  def name =
    forename + " " + surname

  def isPasswordOk(passwd: String) =
    BCrypt.compare(passwd, password.hash)
}

object SimpleUser {
  import JsonFormatters._

  object internalFormat extends JsonFormat[Problem, SimpleUser] with JsonFormatters {
    val valueManifest = manifest[SimpleUser]

    def read(data: JValue) =
      for {
        id        <- data.mandatory[Uuid]("id")
        username  <- data.mandatory[String]("username")
        password  <- data.mandatory[String]("password")
        forenames <- data.mandatory[String]("forenames")
        surname   <- data.mandatory[String]("surname")
        admin     <- data.mandatory[Boolean]("admin")
      } yield SimpleUser(id, username, Password.fromHash(password), forenames, surname, admin)

    def write(user: SimpleUser) =
      ("typename " -> "simpleuser") ~
      ("id"        -> user.id.toJson) ~
      ("username"  -> user.username) ~
      ("forenames" -> user.forenames) ~
      ("surname"   -> user.surname) ~
      ("password"  -> user.password.hash) ~
      ("admin"     -> user.admin)
  }

  object externalFormat extends JsonUpdater[Problem, SimpleUser] with JsonFormatters {
    val valueManifest = manifest[SimpleUser]

    def read(data: JValue) =
      for {
        username  <- data.mandatory[String]("username")
        password  <- data.mandatory[String]("password")
        forenames <- data.mandatory[String]("forenames")
        surname   <- data.mandatory[String]("surname")
        admin     <- data.mandatory[Boolean]("admin")
      } yield SimpleUser(Uuid.create, username, Password.fromPassword(password), forenames, surname, admin)

    def write(user: SimpleUser) =
      ("typename"  -> "simpleuser") ~
      ("id"        -> user.id.toJson) ~
      ("username"  -> user.username) ~
      ("forenames" -> user.forenames) ~
      ("surname"   -> user.surname)

    def update(user: SimpleUser, data: JValue) =
      for {
        username  <- data.optional[String]("username").map(_.getOrElse(user.username))
        password  <- data.optional[String]("password").map(_.map(Password.fromPassword _).getOrElse(user.password))
        forenames <- data.optional[String]("forenames").map(_.getOrElse(user.forenames))
        surname   <- data.optional[String]("surname").map(_.getOrElse(user.surname))
        admin     <- data.optional[Boolean]("admin").map(_.getOrElse(user.admin))
      } yield SimpleUser(user.id, username, password, forenames, surname, admin)
  }
}
