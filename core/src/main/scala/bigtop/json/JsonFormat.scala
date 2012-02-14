package bigtop
package json

import blueeyes.json.JsonAST.JValue
import bigtop.util.{Reader, Writer, Updater, Format}

trait JsonReader[E,I] extends Reader[E,I,JValue]

trait JsonWriter[I] extends Writer[I,JValue]

trait JsonFormat[E,I] extends Format[E,I,JValue] with JsonWriter[I] with JsonReader[E,I]

trait JsonUpdater[E,I] extends Updater[E,I,JValue] with JsonFormat[E,I]
