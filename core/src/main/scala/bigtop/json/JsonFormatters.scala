package bigtop
package json

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

  def asSeq[T](implicit reader: JsonReader[T]): JsonValidation[Seq[T]] = {
    for {
      arr <- (json -->? classOf[JArray]).toSuccess(malformed(JPath.Identity, "array", json))
      ans <- arr.elements.map(reader.read _).sequence[JsonValidation, T]
    } yield ans
  }

  def asMap[T](implicit reader: JsonReader[T]): JsonValidation[Map[String,T]] = {
    for {
      obj <- (json -->? classOf[JObject]).toSuccess(malformed(JPath.Identity, "array", json))
      ans <- obj.fields.map { case JField(name, value) =>
               reader.read(value).map(value => name -> value)
             }.sequence[JsonValidation, (String, T)]
    } yield Map(ans : _*)
  }

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

  // def mandatoryMap[T](path: JPath)(implicit reader: JsonReader[T]): JsonValidation[Map[String, T]] =
  //   mandatory[JValue](path).flatMap(field => prefixErrors(path, field.asMap[T]))

  // def optionalSeq[T](path: JPath)(implicit reader: JsonReader[T]): JsonValidation[Option[Seq[T]]] =
  //   for {
  //     optField <- optional[JValue](path)
  //     optValue <- optField match {
  //                   case Some(field) => prefixErrors(path, field.asSeq[T].map(Some(_)))
  //                   case None        => Option.empty[Seq[T]].success[JsonErrors]
  //                 }
  //   } yield optValue

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

trait JsonFormatters {
  // Atomic Values ---------------------

  implicit val UuidJsonFormat: JsonFormat[Uuid] = new JsonFormat[Uuid] {
    def write(in: Uuid) = JString(in.toString)
    def read(json: JValue) =
      (json -->? classOf[JString]).
        map(_.value).
        flatMap(Uuid.parse _).
        toSuccess(malformed(JPath.Identity, "uuid", json))
  }

  implicit val EmailJsonFormat: JsonFormat[Email] = new JsonFormat[Email] {
    def write(in: Email) = JString(in.address)
    def read(json: JValue) =
      (json -->? classOf[JString]).
        map(_.value).
        flatMap(Email.parse _).
        toSuccess(malformed(JPath.Identity, "email", json))
  }

  implicit val PasswordJsonFormat: JsonFormat[Password] = new JsonFormat[Password] {
    def write(in: Password) = JString(in.hash)
    def read(json: JValue) = {
      (json -->? classOf[JString]).
        map(_.value).
        flatMap(Password.parseHash(_).toOption).
        toSuccess(malformed(JPath.Identity, "password", json))
    }
  }

  implicit val StringJsonFormat: JsonFormat[String] = new JsonFormat[String] {
    def write(in: String) = JString(in)
    def read(json: JValue) =
      (json -->? classOf[JString]).
      map(_.value).
      toSuccess(malformed(JPath.Identity, "string", json))
  }

  implicit val BooleanJsonFormat: JsonFormat[Boolean] = new JsonFormat[Boolean] {
    def write(in: Boolean) = JBool(in)
    def read(json: JValue) =
      (json -->? classOf[JBool]).
      map(_.value).
      toSuccess(malformed(JPath.Identity, "boolean", json))
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
            case Failure(str)  => malformed(JPath.Identity, "time", json).fail[DateTime]
            case Success(date) => date.success[JsonErrors]
          }

        case _ =>
          malformed(JPath.Identity, "time", json).fail[DateTime]
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
      (malformed(JPath.Identity, "int", json))
  }

  implicit val DoubleJsonFormat: JsonFormat[Double] = new JsonFormat[Double] {
    def write(in: Double) = JDouble(in)

    def read(json: JValue) =
      (json -->? classOf[JDouble]).
      map(_.value).
      toSuccess(malformed(JPath.Identity, "double", json))
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

  // Tuples ---------------------------

  private type JV[T] = JsonValidation[T]

  def tuple[A, B](a: => JV[A], b: => JV[B]) =
    (a |@| b).tupled

  def tuple[A, B, C](a: => JV[A], b: => JV[B], c: => JV[C]) =
    (a |@| b |@| c).tupled

  def tuple[A, B, C, D](a: => JV[A], b: => JV[B], c: => JV[C], d: => JV[D]) =
    (a |@| b |@| c |@| d).tupled

  def tuple[A, B, C, D, E](a: => JV[A], b: => JV[B], c: => JV[C], d: => JV[D], e: => JV[E]) =
    (a |@| b |@| c |@| d |@| e).tupled

  def tuple[A, B, C, D, E, F](a: => JV[A], b: => JV[B], c: => JV[C], d: => JV[D], e: => JV[E], f: => JV[F]) =
    (a |@| b |@| c |@| d |@| e |@| f).tupled

  def tuple[A, B, C, D, E, F, G](a: => JV[A], b: => JV[B], c: => JV[C], d: => JV[D], e: => JV[E], f: => JV[F], g: => JV[G]) =
    (a |@| b |@| c |@| d |@| e |@| f |@| g).tupled

  // Seqs -----------------------------

  implicit def buildSeqFormat[A](implicit format: JsonFormat[A], manifest: Manifest[Seq[A]]): JsonFormat[Seq[A]] =
    new JsonFormat[Seq[A]] {
      def write(in: Seq[A]) = JArray(in.map(format.write _).toList)
      def read(json: JValue) =
        json -->? classOf[JArray] toSuccess (malformed(JPath.Identity, "array", json)) flatMap (_.elements.map(format.read _).sequence[JsonValidation, A])
    }

  // Maps ------------------------------

  implicit def MapFormat[A, B](implicit keyFormat: Format[JsonErrors, A, String], valFormat: JsonFormat[B], manifest: Manifest[Map[A, B]]): JsonFormat[Map[A,B]] =
    new JsonFormat[Map[A,B]] {
      def write(in: Map[A,B]) =
        JObject(in.toIterable.foldLeft(Nil: List[JField]){
          (accum, pair) => JField(keyFormat.write(pair._1), valFormat.write(pair._2)) :: accum
        })

      def read(json: JValue) =
        (json -->? classOf[JObject]).
        toSuccess(malformed(JPath.Identity, "object", json)).
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


  // Other -----------------------------

  def malformed(path: JPath, `type`: String, json: JValue): JsonErrors = {
    import blueeyes.json.Printer._
    JsonErrors.Malformed(path, "expected %s, found %s".format(`type`, compact(render(json))))
  }

  def prefixErrors[T](path: JPath, in: JsonValidation[T]): JsonValidation[T] =
    in match {
      case Failure(errors) => (errors prefix path).fail[T]
      case success => success
    }

  case class JsonWritable[A](in: A) {
    def toJson(implicit w: JsonWriter[A]): JValue =
      w.write(in)
  }

  implicit def writableToJsonWritable[A](in: A): JsonWritable[A] =
    JsonWritable[A](in)

  implicit def jsonToJValueW(in: JValue): JValueW = JValueW(in)
}

object JsonFormatters extends JsonFormatters
