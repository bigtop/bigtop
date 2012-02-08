package bigtop
package user

import bigtop.json.{Implicits=>JsonImplicits}
import blueeyes.json.JsonAST._
import blueeyes.json.JsonDSL._
import scalaz._
import Scalaz._

case class Password private(val hash: String) 

object Password {

  def fromHash(hash: String) =
    new Password(hash)

  def fromPassword(password: String) =
    new Password("hash" + password)
}

case class SimpleUser(val username: String, val password: Password) extends User {

  def isPasswordOk(passwd: String) =
    false

}

trait SimpleUserExternalWriter extends Writer[SimpleUser] {
  def write(user: SimpleUser) =
    ("typename" -> "simpleuser") ~
    ("username" -> user.username)
}

trait SimpleUserInternalWriter extends Writer[SimpleUser] {
  def write(user: SimpleUser) =
    ("typename" -> "simpleuser") ~
    ("username" -> user.username) ~
    ("password" -> user.password.hash)
}

trait SimpleUserExternalReader extends Reader[Error,SimpleUser] {
  import JsonImplicits._
  
  def read(data: JValue) =
    for {
      username <- data./[Error,String]("username", ErrorCode.NoUserGiven)
      password <- data./[Error,String]("password", ErrorCode.NoPassword)
    } yield SimpleUser(username, Password.fromPassword(password))
}

trait SimpleUserInternalReader extends Reader[Error,SimpleUser] {
  import JsonImplicits._
  
  def read(data: JValue) =
    for {
      username <- data./[Error,String]("username", ErrorCode.NoUserGiven)
      password <- data./[Error,String]("password", ErrorCode.NoPassword)
    } yield SimpleUser(username, Password.fromHash(password))
}

trait SimpleUserExternalFormat extends Format[Error,SimpleUser] 
     with SimpleUserExternalWriter
     with SimpleUserExternalReader
{
  def update(user: SimpleUser, data: JValue) =
    ("everything" -> "failed").fail.liftFailNel
}

trait SimpleUserExternalImplicits {

  implicit val format = new SimpleUserExternalFormat {}

}

object SimpleUserExternalImplicits extends SimpleUserExternalImplicits
