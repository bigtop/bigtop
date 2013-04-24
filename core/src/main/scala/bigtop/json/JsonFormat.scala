package bigtop.json

import bigtop.data._
import bigtop.util._
import blueeyes.json.JsonAST._
import scalaz._
import scalaz.Scalaz._

object JsonFormats extends JsonFormats

trait JsonFormats extends format.BaseFormats
  with format.SequenceFormats
  with format.MapFormats
  with format.PathFormats
  with format.NullableFormats
  with format.TupleFormats
  with PathFormatImplicits

object JsonReaders extends JsonReaders

trait JsonReaders extends format.BaseReaders
  with format.SequenceReaders
  with format.MapReaders
  with format.PathReaders
  with format.NullableReaders
  with format.TupleReaders
  with PathReaderImplicits
