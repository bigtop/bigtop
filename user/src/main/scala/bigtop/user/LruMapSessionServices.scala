package bigtop
package user

import net.lag.configgy.ConfigMap

class LruMapSessionServices[U <: User](val config:  ConfigMap,
                                       val reader:  SessionReadService[U],
                                       val creator: SessionCreateService[U],
                                       val userActions: UserActions[U]) extends SessionServices[U] {

  val create = creator.create
  val read   = reader.read

}
