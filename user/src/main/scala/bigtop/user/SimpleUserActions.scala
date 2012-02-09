package bigtop
package user

import akka.dispatch.Future
import akka.util.Timeout
import akka.util.duration._
import blueeyes.persistence.mongo._
import blueeyes.json.JsonAST._
import scalaz.{NonEmptyList,Scalaz,Validation}
import Scalaz._
import bigtop.concurrent.{FutureValidation, FutureImplicits}
import net.lag.configgy.ConfigMap

case class SimpleUserConfig(config: ConfigMap, mongo: Mongo) {
  val database   = mongo.database(config("database"))
}

class SimpleUserActions(config: SimpleUserConfig) extends UserActions[SimpleUser] {

  def protocol = new SimpleUserExternalFormat {}

  def store = new SimpleUserStore(config)

}

class SimpleUserStore(config: SimpleUserConfig) extends UserStore[SimpleUser]
    with SimpleUserInternalReader
    with SimpleUserInternalWriter
{
  import FutureImplicits._

  implicit def queryTimeout = Timeout(3.seconds)

  def collection = "users"

  def get(username: String): UserValidation = {
    val user: Future[Option[JObject]] =
      config.database(selectOne()
                      .from(collection)
                      .where("username" === username))

    user map{ u => u.toSuccess("user" -> "Does not exist").liftFailNel.flatMap(read _) }
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


  private def mapOrHandleError[T,S](f: Future[T], mapper: T => S): FutureValidation[NonEmptyList[Error],S] = {
    val ans = new Future[ValidationNEL[Error,S]]
    f foreach { v => ans.deliver(mapper(v).success[Error].liftFailNel) }
    f ifCanceled { e =>
      ans.deliver {
        ("error" ->
         (e map { _.getMessage } getOrElse("Unknown"))).fail[S].liftFailNel
      }
    }

    ans.fv
  }

}
