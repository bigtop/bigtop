package bigtop
package session


import akka.dispatch.Future
import blueeyes.core.service.ServiceContext
import bigtop.user.User


trait SessionServiceFactory[U <: User] {

  type Config

  def setup(context: ServiceContext): Future[Config]

  def create(config: Config): SessionActions[U]

}
