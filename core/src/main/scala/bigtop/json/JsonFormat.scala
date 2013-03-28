package bigtop
package json

import blueeyes.json.JsonAST.JValue
import bigtop.util.{Reader, Writer, Updater, Format}

trait JsonReader[T] extends Reader[JsonErrors, T, JValue]

trait JsonWriter[T] extends Writer[T, JValue]

trait JsonFormat[T] extends Format[JsonErrors, T, JValue] with JsonWriter[T] with JsonReader[T]

trait JsonUpdater[T] extends Updater[JsonErrors, T, JValue] with JsonFormat[T]
