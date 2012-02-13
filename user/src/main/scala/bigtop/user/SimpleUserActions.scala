package bigtop
package user

import akka.dispatch.{Future, Promise}
import akka.util.Timeout
import akka.util.duration._
import blueeyes.persistence.mongo._
import blueeyes.json.JsonAST._
import scalaz.{NonEmptyList, Validation, ValidationNEL}
import scalaz.std.option.optionSyntax._
import scalaz.syntax.validation._
import bigtop.concurrent.{FutureValidation, FutureImplicits}
import bigtop.problem.{Problem, InternalError}
import bigtop.problem.Problems._
import net.lag.configgy.ConfigMap

case class SimpleUserConfig(val config: ConfigMap, val mongo: Mongo) {
  lazy val database = mongo.database(config("database"))
}

class SimpleUserActions(config: SimpleUserConfig) extends UserActions[SimpleUser] {

  def updater = new SimpleUserExternalFormat {}
  def formatter = updater
  def store = new SimpleUserStore(config)

}

class SimpleUserStore(config: SimpleUserConfig) extends UserStore[SimpleUser]
    with SimpleUserInternalReader
    with SimpleUserInternalWriter {
  import FutureImplicits._

  implicit def queryTimeout = Timeout(3.seconds)

  def collection = "users"

  def get(username: String): UserValidation = {
    val user: Future[Option[JObject]] =
      config.database(selectOne()
                      .from(collection)
                      .where("username" === username))

    user map{ u => u.toSuccess(Request.NoUser).flatMap(read _) }
  }

  def add(user: SimpleUser): UserValidation = {
    val result: Future[Unit] =
      config.database(upsert(collection)
                      .set(new MongoUpdateObject(write(user)))
                      .where("username" === user.username))

    mapOrHandleError(result, (_: Unit) => user)
  }


  def delete(username: String): UnitValidation = {
    val result: Future[Unit] =
      config.database(remove.from(collection).where("username" === username))

    mapOrHandleError(result, (_: Unit) => ())
  }


  private def mapOrHandleError[T,S](f: Future[T], mapper: T => S): FutureValidation[Problem[String],S] = {
    val ans = Promise[Validation[Problem[String],S]]
    f foreach { v => ans.success(mapper(v).success[Problem[String]]) }
    f recover { case e =>
      ans.success {
        InternalError(e.getMessage).fail[S]
      }
    }

    ans.fv
  }

}
