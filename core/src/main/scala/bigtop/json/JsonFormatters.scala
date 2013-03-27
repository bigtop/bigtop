package bigtop
package json

import bigtop.util._
import bigtop.problem._
import blueeyes.json.JsonAST._
import blueeyes.json.Printer
import java.net.URL
import org.joda.time._
import scalaz._
import scalaz.Scalaz._

/** A "wide" (i.e. pimped) JSON value */
case class JValueW(json: JValue) {
  import JsonFormatters._

  // def mandatory[T](name: String)(implicit reader: JsonReader[Problem,T]): Validation[Problem,T] =
  //   (json get name) match {
  //     case JNothing => Problems.Missing(name).fail[T]
  //     case other    => reader.read(other)
  //   }

  def mandatory[T](name: String)(implicit reader: JsonReader[Problem,T]): Validation[Problem,T] =
    (json \? name).toSuccess(Problems.Missing(name)).flatMap(reader.read _)

  def optional[T](name: String)(implicit reader: JsonReader[Problem,T]): Validation[Problem,Option[T]] =
    (json \? name) match {
      case Some(json) => reader.read(json).map(Some(_))
      case None       => Option.empty[T].success[Problem]
    }

  def optional[T](name: String, default: T)(implicit reader: JsonReader[Problem,T]): Validation[Problem,T] =
    (json \? name).map(reader.read _).getOrElse(default.success)

  def mandatorySeq[T](name: String)(implicit reader: JsonReader[Problem, T]): Validation[Problem,Seq[T]] =
    mandatory[JValue](name).flatMap(field => JValueW(field).asSeq[T](name))

  def mandatoryMap[T](name: String)(implicit reader: JsonReader[Problem, T]): Validation[Problem,Map[String,T]] =
    mandatory[JValue](name).flatMap(field => JValueW(field).asMap[T](name))

  def optionalSeq[T](name: String)(implicit reader: JsonReader[Problem, T]): Validation[Problem,Option[Seq[T]]] =
    (json \? name) match {
      case Some(json) => json.asSeq[T](name).map(Some(_))
      case None       => Option.empty[Seq[T]].success[Problem]
    }

  def optionalMap[T](name: String)(implicit reader: JsonReader[Problem, T]): Validation[Problem,Option[Map[String,T]]] =
    (json \? name) match {
      case Some(json) => json.asMap[T](name).map(Some(_))
      case None       => Option.empty[Map[String,T]].success[Problem]
    }

  def optionalSeq[T](name: String, default: Seq[T])(implicit reader: JsonReader[Problem, T]): Validation[Problem,Seq[T]] =
    (json \? name).map(_.asSeq[T](name)).getOrElse(default.success)

  def as[T](implicit reader: JsonReader[Problem,T]): Validation[Problem,T] =
    reader.read(json)

  def asSeq[T](name: String = "array")(implicit reader: JsonReader[Problem,T]): Validation[Problem,Seq[T]] = {
    type ValSeq[T] = Validation[Problem,T]
    for {
      arr <- (json -->? classOf[JArray]).toSuccess(malformed(name, "array", json))
      ans <- arr.elements.map(reader.read _).sequence[ValSeq, T]
    } yield ans
  }

  def asMap[T](name: String = "object")(implicit reader: JsonReader[Problem,T]): Validation[Problem,Map[String,T]] = {
    type ValSeq[T] = Validation[Problem,T]
    for {
      obj <- (json -->? classOf[JObject]).toSuccess(malformed(name, "array", json))
      ans <- obj.fields.map { case JField(name, value) => reader.read(value).map(value => name -> value) }.sequence[ValSeq, (String, T)]
    } yield Map(ans : _*)
  }
}

trait JsonFormatters {
  import Problems._

  // Atomic Values ---------------------

  implicit val UuidJsonFormat: JsonFormat[Problem,Uuid] = new JsonFormat[Problem,Uuid] {
    def write(in: Uuid) = JString(in.toString)
    def read(json: JValue) =
      json -->? classOf[JString] map (_.value) flatMap (Uuid.parse _) toSuccess (malformed("json", "uuid", json))
  }

  implicit val EmailJsonFormat: JsonFormat[Problem,Email] = new JsonFormat[Problem,Email] {
    def write(in: Email) = JString(in.address)
    def read(json: JValue) =
      (json -->? classOf[JString]).
        map(_.value).
        flatMap(Email.parse _).
        toSuccess(malformed("json", "email", json))
  }

  implicit val PasswordJsonFormat: JsonFormat[Problem,Password] = new JsonFormat[Problem,Password] {
    def write(in: Password) = JString(in.hash)
    def read(json: JValue) = {
      (json -->? classOf[JString]).
        map(_.value).
        flatMap(Password.parseHash(_).toOption).
        toSuccess(malformed("json", "password", json))
    }
  }

  implicit val StringJsonFormat: JsonFormat[Problem,String] = new JsonFormat[Problem,String] {
    def write(in: String) = JString(in)
    def read(json: JValue) =
      json -->? classOf[JString] map (_.value) toSuccess (malformed("json", "string", json))
  }

  implicit val BooleanJsonFormat: JsonFormat[Problem,Boolean] = new JsonFormat[Problem,Boolean] {
    def write(in: Boolean) = JBool(in)
    def read(json: JValue) =
      json -->? classOf[JBool] map (_.value) toSuccess (malformed("json", "boolean", json))
  }

  implicit val DateTimeJsonFormat: JsonFormat[Problem,DateTime] = new JsonFormat[Problem,DateTime] {
    def write(in: DateTime) = {
      // JInt(in.getMillis)
      JString(Iso8601Format.write(in))
    }

    def read(json: JValue) = {
      json match {
        // case JInt(bigInt) =>
        //   new DateTime(bigInt.toLong).success[Problem]

        // case JDouble(bigDecimal) =>
        //   new DateTime(bigDecimal.toLong).success[Problem]

        case JString(str) =>
          Iso8601Format.read(str) match {
            case Failure(str)  => malformed("json", "time", json).fail[DateTime]
            case Success(date) => date.success[Problem]
          }

        case _ =>
          malformed("json", "time", json).fail[DateTime]
      }
    }
  }

  implicit val IntJsonFormat: JsonFormat[Problem,Int] = new JsonFormat[Problem,Int] {
    def isWholeNumber(num: Double) = {
      math.abs(num - math.round(num)) < 0.01
    }

    def write(in: Int) = JInt(in)
    def read(json: JValue) =
      (json -->? classOf[JInt] map (_.value.toInt)) orElse
      (json -->? classOf[JDouble] filter (json => isWholeNumber(json.value)) map (_.value.toInt)) toSuccess
      (malformed("json", "int", json))
  }

  implicit val DoubleJsonFormat: JsonFormat[Problem,Double] = new JsonFormat[Problem,Double] {
    def write(in: Double) = JDouble(in)
    def read(json: JValue) =
      json -->? classOf[JDouble] map (_.value) toSuccess (malformed("json", "double", json))
  }

  implicit val URLJsonWriter: JsonWriter[URL] = new JsonWriter[URL] {
    def write(in: URL) = JString(in.toString)
  }

  implicit val JsonConfigJsonFormat = new JsonFormat[Problem, JsonConfig] {
    def write(in: JsonConfig) = in.data
    def read(json: JValue) = JsonConfig(json).success[Problem]
  }

  implicit val JValueJsonFormat: JsonFormat[Problem,JValue] = new JsonFormat[Problem,JValue] {
    def write(in: JValue) = in
    def read(json: JValue) = json.success[Problem]
  }

  // Seq Values ------------------------

  // implicit val SeqUuidJsonFormat: JsonFormat[Problem,Seq[Uuid]] = buildSeqFormat(UuidJsonFormat)
  // implicit val SeqEmailJsonFormat: JsonFormat[Problem,Seq[Email]] = buildSeqFormat(EmailJsonFormat)
  // implicit val SeqStringJsonFormat: JsonFormat[Problem,Seq[String]] = buildSeqFormat(StringJsonFormat)
  // implicit val SeqBooleanJsonFormat: JsonFormat[Problem,Seq[Boolean]] = buildSeqFormat(BooleanJsonFormat)
  // implicit val SeqIntJsonFormat: JsonFormat[Problem,Seq[Int]] = buildSeqFormat(IntJsonFormat)
  // implicit val SeqDoubleJsonFormat: JsonFormat[Problem,Seq[Double]] = buildSeqFormat(DoubleJsonFormat)

  implicit def buildSeqFormat[A](implicit format: JsonFormat[Problem,A], manifest: Manifest[Seq[A]]): JsonFormat[Problem,Seq[A]] =
    new JsonFormat[Problem,Seq[A]] {
      def write(in: Seq[A]) = JArray(in.map(format.write _).toList)
      def read(json: JValue) =
        json -->? classOf[JArray] toSuccess (malformed("json", "array", json)) flatMap (_.elements.map(format.read _).sequence[({type l[A] = Validation[Problem, A]})#l, A])
    }

  // Map -------------------------------

  implicit def MapFormat[A, B](implicit keyFormat: Format[Problem, A, String], valFormat: JsonFormat[Problem, B], manifest: Manifest[Map[A, B]]): JsonFormat[Problem, Map[A,B]] =
    new JsonFormat[Problem, Map[A,B]] {
      def write(in: Map[A,B]) =
        JObject(in.toIterable.foldLeft(Nil: List[JField]){
          (accum, pair) => JField(keyFormat.write(pair._1), valFormat.write(pair._2)) :: accum
        })

      def read(json: JValue) =
        json -->? classOf[JObject] toSuccess (malformed("json", "object", json)) flatMap { obj =>
          for {
            fields <- obj.fields.foldLeft(Nil: List[Validation[Problem,(A,B)]]) { (accum, field) =>
                        (for {
                          key   <- keyFormat.read(field.name)
                          value <- valFormat.read(field.value)
                        } yield (key -> value)) :: accum
                      }.sequence[({type l[C] = Validation[Problem,C]})#l, (A,B)]
          } yield Map[A,B]() ++ fields
        }
    }


  // Other -----------------------------

  def malformed(name: String, `type`: String, json: JValue) = {
    import blueeyes.json.Printer._
    Problems.Malformed(name, "expected %s, found %s".format(`type`, compact(render(json))))
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
