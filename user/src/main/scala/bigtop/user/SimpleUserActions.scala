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
import scalaz.{NonEmptyList, Validation}
import scalaz.std.option.optionSyntax._
import scalaz.syntax.validation._

case class SimpleUserActions[U <: User](
  val config: Configuration,
  val internalFormat: JsonFormat[U]
) extends UserActions[U]
  with ConfigurableMongo
  with JsonFormatters
  with MongoImplicits
  with FutureImplicits
{
  lazy val mongoConfig = config.detach("mongo")
  lazy val mongoFacade = mongo(mongoConfig)
  lazy val database = mongoFacade.database(config[String]("mongo.database"))
  lazy val collection = "users"

  implicit def queryTimeout = Timeout(3.seconds)

  def read(id: Uuid): UserValidation = {
    for {
      json <- database(
                selectOne().
                from(collection).
                where("id" === id.toJson)
              ).map(_.toSuccess(Problems.NotFound("user"))).fv
      user <- toServerProblem(internalFormat.read(json))
    } yield user
  }

  def readByUsername(username: String): UserValidation = {
    for {
      json <- database(
                selectOne().
                from(collection).
                where("username" === username.toJson)
              ).map(_.toSuccess(Problems.NotFound("user"))).fv
      user <- toServerProblem(internalFormat.read(json))
    } yield user
  }

  def save(user: U): UserValidation = {
    for {
      data   <- internalFormat.write(user) match {
                  case obj: JObject => obj.success[Problem].fv
                  case other        => Problems.Unknown(
                                         message    = "Could not save the user.",
                                         logMessage = Some("Could not save the user: the internalFormat didn't produce a JObject.")
                                       ).fail[JObject].fv
                }
      result <- database(
                  upsert(collection).
                  set(data).
                  where("id" === user.id.toJson)
                ).map(unit => user.success[Problem]).fv
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

  def mapOrHandleError[T,S](f: Future[T], mapper: T => S): FutureValidation[S] = {
    val ans = Promise[Validation[Problem,S]]
    f foreach { v => ans.success(mapper(v).success[Problem]) }
    f recover { case e =>
      ans.success {
        Problems.Unknown(
          logMessage = Some(e.getMessage),
          cause      = Some(e)
        ).fail[S]
      }
    }

    ans.fv
  }
}
