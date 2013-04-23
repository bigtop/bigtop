package bigtop.json

import blueeyes.json.JsonAST._
import bigtop.util._

trait JsonUpdater[T] extends Updater[JsonErrors, T, JValue] with JsonFormat[T]
