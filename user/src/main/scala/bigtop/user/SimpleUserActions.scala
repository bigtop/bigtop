package bigtop
package user

import akka.dispatch.{Future, Promise}
import akka.util.Timeout
import akka.util.duration._
import bigtop.concurrent.{FutureValidation, FutureImplicits}
import bigtop.json._
import bigtop.problem._
import bigtop.problem.Problems._
import bigtop.util.Uuid
import blueeyes.persistence.mongo._
import blueeyes.json.JsonAST._
import com.weiglewilczek.slf4s.Logging
import org.streum.configrity.Configuration
import scalaz.{NonEmptyList, Validation, ValidationNEL}
import scalaz.std.option.optionSyntax._
import scalaz.syntax.validation._

object SimpleUserActionsBuilder extends ConfigurableMongo {

  def apply(config: Configuration): UserActions[SimpleUser] = {
    lazy val mongoConfig = config.detach("mongo")
    lazy val mongoFacade = mongo(mongoConfig)
    lazy val database = mongoFacade.database(config[String]("mongo.database"))

    val externalFormat = SimpleUser.externalFormat
    lazy val store = new SimpleUserStore(database)

    UserActionsBuilder(store, externalFormat)
  }

}

class SimpleUserStore(database: Database) extends UserStore[SimpleUser] with JsonFormatters with MongoImplicits {
  import FutureImplicits._

  implicit def queryTimeout = Timeout(3.seconds)

  def collection = "users"

  implicit val internalFormat = SimpleUser.internalFormat

  private def mapOrHandleError[T,S](f: Future[T], mapper: T => S): FutureValidation[Problem,S] = {
    val ans = Promise[Validation[Problem,S]]
    f foreach { v => ans.success(mapper(v).success[Problem]) }
    f recover { case e =>
      ans.success {
        ServerProblem(e.getMessage).fail[S]
      }
    }

    ans.fv
  }

  def create(user: SimpleUser): UserValidation = {
    val result: Future[Unit] =
      database(insert(internalFormat.write(user)).into(collection))

    mapOrHandleError(result, (_: Unit) => user)
  }

  def read(id: Uuid): UserValidation = {
    val user: Future[Option[JObject]] =
      database(selectOne().from(collection).where("id" === id.toJson))

    user map { u => u.toSuccess(Client.notFound("user")).flatMap(internalFormat.read _) }
  }

  def update(user: SimpleUser): UserValidation = {
    val result: Future[Unit] =
      database(MongoImplicits.update(collection)
                      .set(new MongoUpdateObject(internalFormat.write(user)))
                      .where("username" === user.username))
    mapOrHandleError(result, (_: Unit) => user)
  }

  def delete(id: Uuid): UnitValidation = {
    val result: Future[Unit] =
      database(remove.from(collection).where("id" === id.toJson))

    mapOrHandleError(result, (_: Unit) => ())
  }

  def searchByUsername(username: String): UserValidation = {
    val user: Future[Option[JObject]] =
      database(selectOne().from(collection).where("username" === username))

    user map { u => u.toSuccess(Client.notFound("user")).flatMap(internalFormat.read _) }
  }
}
