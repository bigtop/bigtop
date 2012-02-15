// package bigtop
// package user
//
//
// import akka.dispatch.{Future, Promise}
// import bigtop.user._
// import blueeyes.persistence.mongo.ConfigurableMongo
// import blueeyes.core.service.ServiceContext
// import blueeyes.bkka.AkkaDefaults
//
//
// object SimpleUserSessionActionsFactory extends SessionActionsFactory[SimpleUser]
//     with ConfigurableMongo
//     with AkkaDefaults
// {
//
//   type Config = SimpleUserActionsConfig
//
//   def setup(context: ServiceContext) = {
//     val config = SimpleUserActionsConfig(context.config)
//     Promise.successful(config)
//   }
//
//   def create(config: SimpleUserActionsConfig) = {
//     val simpleUserActions = new SimpleUserActions(config)
//     val sessionActions = new SessionActions[SimpleUser] {
//       val userActions = simpleUserActions
//     }
//
//     sessionActions
//   }
//
// }
//
// class SimpleUserSessionService extends SessionService[SimpleUser] {
//
//   val sessionActionsFactory = SimpleUserSessionActionsFactory
//
// }
