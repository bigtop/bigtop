package bigtop
package json

import bigtop.util._
import bigtop.problem._
import blueeyes.json.JsonAST._
import java.net.URL
import org.joda.time._
import scalaz.{Failure,Success,Validation}
import scalaz.syntax.validation._
import scalaz.std.list._
import scalaz.syntax.traverse._
import scalaz.std.option.optionSyntax._

/** A "wide" (i.e. pimped) JSON value */
case class JValueW(json: JValue) {
  import Problems._
  import JsonFormatters._

  def mandatory[T](name: String)(implicit reader: JsonReader[Problem,T]): Validation[Problem,T] =
    (json \? name).toSuccess(Client.missing(name)).flatMap(reader.read _)

  def optional[T](name: String)(implicit reader: JsonReader[Problem,T]): Validation[Problem,Option[T]] =
    (json \? name) match {
      case Some(json) => reader.read(json).map(Some(_))
      case None       => Option.empty[T].success[Problem]
    }

  def optional[T](name: String, default: T)(implicit reader: JsonReader[Problem,T]): Validation[Problem,T] =
    (json \? name).map(reader.read _).getOrElse(default.success)

  def mandatorySeq[T](name: String)(implicit reader: JsonReader[Problem, T]): Validation[Problem,Seq[T]] =
    mandatory[JValue](name).flatMap(field => JValueW(field).asSeq[T])

  def optionalSeq[T](name: String)(implicit reader: JsonReader[Problem, T]): Validation[Problem,Option[Seq[T]]] =
    (json \? name) match {
      case Some(json) => json.asSeq[T].map(Some(_))
      case None       => Option.empty[Seq[T]].success[Problem]
    }

  def optionalSeq[T](name: String, default: Seq[T])(implicit reader: JsonReader[Problem, T]): Validation[Problem,Seq[T]] =
    (json \? name).map(_.asSeq[T]).getOrElse(default.success)

  def as[T](implicit reader: JsonReader[Problem,T]): Validation[Problem,T] =
    reader.read(json)

  def asSeq[T](implicit reader: JsonReader[Problem, T]): Validation[Problem,Seq[T]] = {
    type ValSeq[T] = Validation[Problem, T]
    for {
      array <- (json -->? classOf[JArray]).toSuccess(Problems.Client.malformed("array", "expected array, received something else"))
      ans   <- array.elements.map(reader.read _).sequence[ValSeq, T]
    } yield ans
  }
}

trait JsonFormatters {
  import Problems._

  // Atomic Values ---------------------

  implicit val UuidJsonFormat: JsonFormat[Problem,Uuid] = new JsonFormat[Problem,Uuid] {
    def write(in: Uuid) = JString(in.name)
    def read(json: JValue) =
      json -->? classOf[JString] map (_.value) flatMap (Uuid.parse _) toSuccess (malformed("uuid", json))
  }

  implicit val EmailJsonFormat: JsonFormat[Problem,Email] = new JsonFormat[Problem,Email] {
    def write(in: Email) = JString(in.address)
    def read(json: JValue) =
      json -->? classOf[JString] map (_.value) flatMap (Email.parse _) toSuccess (malformed("email", json))
  }

  implicit val StringJsonFormat: JsonFormat[Problem,String] = new JsonFormat[Problem,String] {
    def write(in: String) = JString(in)
    def read(json: JValue) =
      json -->? classOf[JString] map (_.value) toSuccess (malformed("string", json))
  }

  implicit val BooleanJsonFormat: JsonFormat[Problem,Boolean] = new JsonFormat[Problem,Boolean] {
    def write(in: Boolean) = JBool(in)
    def read(json: JValue) =
      json -->? classOf[JBool] map (_.value) toSuccess (malformed("boolean", json))
  }

  implicit val DateTimeJsonFormat: JsonFormat[Problem,DateTime] = new JsonFormat[Problem,DateTime] {
    def write(in: DateTime) = JString(Iso8601Format.write(in))
    def read(json: JValue) =
      json -->? classOf[JString] map (_.value) toSuccess (malformed("time", json)) flatMap (Iso8601Format.read(_)) match {
        case Failure(msg) => (malformed("time", json)).fail
        case Success(s)   => s.success
      }
  }

  implicit val IntJsonFormat: JsonFormat[Problem,Int] = new JsonFormat[Problem,Int] {
    def write(in: Int) = JInt(in)
    def read(json: JValue) =
      json -->? classOf[JInt] map (_.value.toInt) toSuccess (malformed("int", json))
  }

  implicit val DoubleJsonFormat: JsonFormat[Problem,Double] = new JsonFormat[Problem,Double] {
    def write(in: Double) = JDouble(in)
    def read(json: JValue) =
      json -->? classOf[JDouble] map (_.value) toSuccess (malformed("double", json))
  }

  implicit val URLJsonWriter: JsonWriter[URL] = new JsonWriter[URL] {
    def write(in: URL) = JString(in.toString)
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

  implicit def buildSeqFormat[A](implicit format: JsonFormat[Problem,A]): JsonFormat[Problem,Seq[A]] =
    new JsonFormat[Problem,Seq[A]] {
      def write(in: Seq[A]) = JArray(in.map(format.write _).toList)
      def read(json: JValue) =
        json -->? classOf[JArray] toSuccess (malformed("array", json)) flatMap (_.elements.map(format.read _).sequence[({type l[A] = Validation[Problem,A]})#l, A])
    }

  // Map -------------------------------

  implicit def MapFormat[A,B](implicit keyFormat: Format[Problem, A, String], valFormat: JsonFormat[Problem, B]): JsonFormat[Problem, Map[A,B]] =
    new JsonFormat[Problem, Map[A,B]] {
      def write(in: Map[A,B]) =
        JObject(in.toIterable.foldLeft(Nil: List[JField]){
          (accum, pair) => JField(keyFormat.write(pair._1), valFormat.write(pair._2)) :: accum
        })

      def read(json: JValue) =
        json -->? classOf[JObject] toSuccess (malformed("object", json)) flatMap { obj =>
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

  def malformed(`type`: String, json: JValue) = {
    import blueeyes.json.Printer._
    Client.malformed("data", "expected %s, found %s".format(`type`, compact(render(json))))
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
