package bigtop
package user

import org.streum.configrity.Configuration

class LruMapSessionServices[U <: User](
  val config:      Configuration,
  val reader:      SessionReadService[U],
  val creator:     SessionCreateService[U],
  val switch:      SessionSwitchUserService[U],
  val userActions: UserActions[U]
) extends SessionServices[U] {

  val create     = creator.create
  val read       = reader.read
  val switchUser = switch.switchUser

}
