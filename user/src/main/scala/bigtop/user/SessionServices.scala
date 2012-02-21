package bigtop
package user

import bigtop.http.JsonServiceImplicits

trait SessionCreateService[U <: User] extends JsonHttpService with JsonServiceImplicits {

  def action: SessionCreate[U]

  val service =
    (req: HttpRequest[Future[JValue]]) =>
      for {
        json     <- getContent(req)
        username <- json.mandatory[String]("username").fv
        password <- json.mandatory[String]("password").fv
        result   <- action.create(username, password)
      } yield result

}

trait SessionReadService[U <: User] {

  def action: SessionRead[U]

respond {
          req =>
            for {
              id      <- getId(req)
              session <- sessionActions.read(id)
            } yield session
        }
}
