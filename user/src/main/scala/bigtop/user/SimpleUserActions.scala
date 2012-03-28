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

case class SimpleUserActions(val config: Configuration) extends UserActions[SimpleUser]
  with ConfigurableMongo
  with JsonFormatters
  with MongoImplicits
  with FutureImplicits
{
  lazy val mongoConfig = config.detach("mongo")
  lazy val mongoFacade = mongo(mongoConfig)
  lazy val database = mongoFacade.database(config[String]("mongo.database"))
  lazy val collection = "users"

  lazy val internalFormat = SimpleUser.internalFormat

  implicit def queryTimeout = Timeout(3.seconds)

  def read(id: Uuid): UserValidation = {
    val user: Future[Option[JObject]] =
      database(
        selectOne().
        from(collection).
        where("id" === id.toJson)
      )

    user map { u =>
      u.toSuccess(Client.notFound("user")).flatMap(internalFormat.read _)
    }
  }

  def readByUsername(username: String): UserValidation = {
    val user: Future[Option[JObject]] =
      database(
        selectOne().
        from(collection).
        where("username" === username.toJson)
      )

    user map { u =>
      u.toSuccess(Client.notFound("user")).flatMap(internalFormat.read _)
    }
  }

  def save(user: SimpleUser): UserValidation = {
    for {
      data: JObject <- internalFormat.write(user) match {
                         case JObject(fields) => JObject(fields.filter(_.name != "id")).success[Problem].fv
                         case _               => Problems.Server.unknown("could not save user: internal format did not produce a JObject").fail[JObject].fv
                       }
      result        <- database(
                         upsert(collection).
                         set(data).
                         where("id" === user.id.toJson)
                       ).map(_ => user.success[Problem]).fv
    } yield result
  }

  def delete(id: Uuid): UnitValidation = {
    val result: Future[Unit] =
      database(
        remove.
        from(collection).
        where("id" === id.toJson)
      )

    mapOrHandleError(result, (_: Unit) => ())
  }

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
}
