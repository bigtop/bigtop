// package bigtop
// package session
//
// import akka.dispatch.{Future, Promise}
// import bigtop.concurrent._
// import bigtop.json.{JsonWriter, JsonFormatters}
// import bigtop.problem._
// import bigtop.user.{UserActions, User}
// import bigtop.util.Uuid
// import blueeyes.core.http._
// import blueeyes.core.service.ServiceContext
// import blueeyes.persistence.mongo._
// import blueeyes.json.JsonAST._
// import blueeyes.json.JsonDSL._
// import com.twitter.util.{LruMap,SynchronizedLruMap}
// import net.lag.configgy.ConfigMap
// import net.lag.logging.Logger
// import scala.collection.mutable.HashMap
//
// trait SessionAuth[U <: User] extends SessionCore[U] with FutureImplicits {
//
//   type FV[T] = FutureValidation[Problem, T]
//
//   def userActions: UserActions[U]
//
//   def withSession[T](sessionId: Uuid)(body: (Option[Session[U]]) => FV[T]): FV[T] =
//     sessionStore.get(sessionId(request)).map(body)
//
//   def authenticate[T](request: HttpRequest[_])(body: (U) => FV[T]): FV[T] =
//     withSession { session: Session[U] =>
//       body(session.user)
//     }
//
//   def authorize[T](user: U, isAuthorized: U => Boolean)(body: => FV[T]) =
//     if(isAuthorized(user))
//       body
//     else
//       Problems.Client.NotAuthorized.fail[T].fv
//
// }
