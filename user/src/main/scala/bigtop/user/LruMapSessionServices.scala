package bigtop
package user

import net.lag.configgy.ConfigMap

class LruMapSessionServices[U <: User](val config: ConfigMap, val userActions: UserActions[U]) extends SessionServices[U] {

  override val action = new LruMapSessionActions[U](config, userActions)

  def authorizer = new SessionCookieAuthorizer[U] { def action = this.action }

}
