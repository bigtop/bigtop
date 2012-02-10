package bigtop
package user


import akka.dispatch.Future
import blueeyes.core.service.ServiceContext


trait UserActionsFactory[U <: User] {

  /** The configuration necessary to construct a UserActions of type A */
  type Config

  def setup(context: ServiceContext): Future[Config]

  def create(config: Config): UserActions[U]

}
