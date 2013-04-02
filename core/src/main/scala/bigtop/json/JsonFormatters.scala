package bigtop
package json

import bigtop.concurrent._
import bigtop.problem._
import bigtop.util._
import blueeyes.json._
import blueeyes.json.JsonAST._
import blueeyes.json.Printer
import java.net.URL
import org.joda.time._
import scalaz._
import scalaz.Scalaz._

/** A "wide" (i.e. pimped) JSON value */
case class JValueW(json: JValue) {
  import JsonFormatters._

  def as[T](implicit reader: JsonReader[T]): JsonValidation[T] =
    reader.read(json)

  def asSeq[T](implicit reader: JsonReader[T]): JsonValidation[Seq[T]] =
    for {
      arr <- (json -->? classOf[JArray]).toSuccess(malformed("array", json))
      ans <- arr.elements.map(reader.read _).sequence[JsonValidation, T]
    } yield ans

  def asMap[T](implicit reader: JsonReader[T]): JsonValidation[Map[String,T]] =
    for {
      obj <- (json -->? classOf[JObject]).toSuccess(malformed("array", json))
      ans <- obj.fields.map { case JField(name, value) =>
               reader.read(value).map(value => name -> value)
             }.sequence[JsonValidation, (String, T)]
    } yield Map(ans : _*)

  def mandatory[T](path: JPath)(implicit reader: JsonReader[T]): JsonValidation[T] =
    (json get path) match {
      case JNothing => JsonErrors.Missing(path).fail[T]
      case other    => prefixErrors(path, reader.read(other))
    }

  def optional[T](path: JPath)(implicit reader: JsonReader[T]): JsonValidation[Option[T]] =
    (json get path) match {
      case JNothing => Option.empty[T].success[JsonErrors]
      case other    => prefixErrors(path, reader.read(other).map(Some(_)))
    }

  def optional[T](path: JPath, default: T)(implicit reader: JsonReader[T]): JsonValidation[T] =
    (json get path) match {
      case JNothing => default.success[JsonErrors]
      case other    => prefixErrors(path, reader.read(other))
    }

  // TODO: Work out if we NEED these. If we do, uncomment them and TEST THEM:

  // def mandatorySeq[T](path: JPath)(implicit reader: JsonReader[T]): JsonValidation[Seq[T]] =
  //   mandatory[JValue](path).flatMap(field => prefixErrors(path, field.asSeq[T]))

  def mandatoryMap[T](path: JPath)(implicit reader: JsonReader[T]): JsonValidation[Map[String, T]] =
    mandatory[JValue](path).flatMap(field => prefixErrors(path, field.asMap[T]))

  def optionalSeq[T](path: JPath)(implicit reader: JsonReader[T]): JsonValidation[Option[Seq[T]]] =
    for {
      optField <- optional[JValue](path)
      optValue <- optField match {
                    case Some(field) => prefixErrors(path, field.asSeq[T].map(Some(_)))
                    case None        => Option.empty[Seq[T]].success[JsonErrors]
                  }
    } yield optValue

  // def optionalSeq[T](path: JPath, default: Seq[T])(implicit reader: JsonReader[T]): JsonValidation[Seq[T]] =
  //   for {
  //     optField <- optional[JValue](path)
  //     optValue <- optField match {
  //                   case Some(field) => prefixErrors(path, field.asSeq[T])
  //                   case None        => default.success[JsonErrors]
  //                 }
  //   } yield optValue

  // def optionalMap[T](path: JPath)(implicit reader: JsonReader[T]): JsonValidation[Option[Map[String,T]]] =
  //   for {
  //     optField <- optional[JValue](path)
  //     optValue <- optField match {
  //                   case Some(field) => prefixErrors(path, field.asMap[T].map(Some(_)))
  //                   case None        => Option.empty[Map[String, T]].success[JsonErrors]
  //                 }
  //   } yield optValue

  // def optionalMap[T](path: JPath, default: Map[String, T])(implicit reader: JsonReader[T]): JsonValidation[Map[String, T]] =
  //   for {
  //     optField <- optional[JValue](path)
  //     optValue <- optField match {
  //                   case Some(field) => prefixErrors(path, field.asMap[T])
  //                   case None        => default.success[JsonErrors]
  //                 }
  //   } yield optValue
}

object JsonFormatters extends JsonFormatters

trait JsonFormatters extends JsonValidationCombinators {

  // Default formats ------------------

  implicit val UuidJsonFormat: JsonFormat[Uuid] = new JsonFormat[Uuid] {
    def write(in: Uuid) = JString(in.toString)
    def read(json: JValue) =
      (json -->? classOf[JString]).
        map(_.value).
        flatMap(Uuid.parse _).
        toSuccess(malformed("uuid", json))
  }

  implicit val EmailJsonFormat: JsonFormat[Email] = new JsonFormat[Email] {
    def write(in: Email) = JString(in.address)
    def read(json: JValue) =
      (json -->? classOf[JString]).
        map(_.value).
        flatMap(Email.parse _).
        toSuccess(malformed("email", json))
  }

  implicit val PasswordJsonFormat: JsonFormat[Password] = new JsonFormat[Password] {
    def write(in: Password) = JString(in.hash)
    def read(json: JValue) = {
      (json -->? classOf[JString]).
        map(_.value).
        flatMap(Password.parseHash(_).toOption).
        toSuccess(malformed("password", json))
    }
  }

  implicit val StringJsonFormat: JsonFormat[String] = new JsonFormat[String] {
    def write(in: String) = JString(in)
    def read(json: JValue) =
      (json -->? classOf[JString]).
      map(_.value).
      toSuccess(malformed("string", json))
  }

  implicit val BooleanJsonFormat: JsonFormat[Boolean] = new JsonFormat[Boolean] {
    def write(in: Boolean) = JBool(in)
    def read(json: JValue) =
      (json -->? classOf[JBool]).
      map(_.value).
      toSuccess(malformed("boolean", json))
  }

  implicit val DateTimeJsonFormat: JsonFormat[DateTime] = new JsonFormat[DateTime] {
    def write(in: DateTime) = {
      // JInt(in.getMillis)
      JString(Iso8601Format.write(in))
    }

    def read(json: JValue) = {
      json match {
        // case JInt(bigInt) =>
        //   new DateTime(bigInt.toLong).success[JsonErrors]

        // case JDouble(bigDecimal) =>
        //   new DateTime(bigDecimal.toLong).success[JsonErrors]

        case JString(str) =>
          Iso8601Format.read(str) match {
            case Failure(str)  => malformed("time", json).fail[DateTime]
            case Success(date) => date.success[JsonErrors]
          }

        case _ =>
          malformed("time", json).fail[DateTime]
      }
    }
  }

  implicit val IntJsonFormat: JsonFormat[Int] = new JsonFormat[Int] {
    def isWholeNumber(num: Double) =
      math.abs(num - math.round(num)) < 0.01

    def write(in: Int) = JInt(in)

    def read(json: JValue) =
      (json -->? classOf[JInt] map (_.value.toInt)) orElse
      (json -->? classOf[JDouble] filter (json => isWholeNumber(json.value)) map (_.value.toInt)) toSuccess
      (malformed("int", json))
  }

  implicit val DoubleJsonFormat: JsonFormat[Double] = new JsonFormat[Double] {
    def write(in: Double) = JDouble(in)

    def read(json: JValue) =
      (json -->? classOf[JDouble]).
      map(_.value).
      toSuccess(malformed("double", json))
  }

  implicit val URLJsonWriter: JsonWriter[URL] = new JsonWriter[URL] {
    def write(in: URL) = JString(in.toString)
  }

  implicit val JsonConfigJsonFormat = new JsonFormat[JsonConfig] {
    def write(in: JsonConfig) = in.data
    def read(json: JValue) = JsonConfig(json).success[JsonErrors]
  }

  implicit val JValueJsonFormat: JsonFormat[JValue] = new JsonFormat[JValue] {
    def write(in: JValue) = in
    def read(json: JValue) = json.success[JsonErrors]
  }

  // Seqs -----------------------------

  implicit def buildSeqFormat[A](implicit format: JsonFormat[A]): JsonFormat[Seq[A]] =
    new JsonFormat[Seq[A]] {
      def write(in: Seq[A]) = JArray(in.map(format.write _).toList)
      def read(json: JValue) =
        (json -->? classOf[JArray]).
        toSuccess(malformed("array", json)).
        flatMap(_.elements.map(format.read _).sequence[JsonValidation, A])
    }

  // Maps ------------------------------

  implicit def MapFormat[A, B](implicit keyFormat: Format[JsonErrors, A, String], valFormat: JsonFormat[B]): JsonFormat[Map[A,B]] =
    new JsonFormat[Map[A,B]] {
      def write(in: Map[A,B]) =
        JObject(in.toIterable.foldLeft(Nil: List[JField]){
          (accum, pair) => JField(keyFormat.write(pair._1), valFormat.write(pair._2)) :: accum
        })

      def read(json: JValue) =
        (json -->? classOf[JObject]).
        toSuccess(malformed("object", json)).
        flatMap { obj =>
          for {
            fields <- obj.fields.foldLeft(Nil: List[JsonValidation[(A, B)]]) { (accum, field) =>
                        (for {
                          key   <- keyFormat.read(field.name)
                          value <- valFormat.read(field.value)
                        } yield (key -> value)) :: accum
                      }.sequence[JsonValidation, (A, B)]
          } yield Map[A,B]() ++ fields
        }
    }

  case class JsonWritable[A](in: A) {
    def toJson(implicit w: JsonWriter[A]): JValue =
      w.write(in)
  }

  implicit def writableToJsonWritable[A](in: A): JsonWritable[A] =
    JsonWritable[A](in)

  implicit def jsonToJValueW(in: JValue): JValueW = JValueW(in)
}
