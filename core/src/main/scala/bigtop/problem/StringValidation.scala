// package bigtop
// package problem

// import akka.dispatch.{ExecutionContext, Promise}
// import blueeyes.bkka.AkkaDefaults
// import bigtop.concurrent._
// import bigtop.problem._
// import scala.util.matching.Regex
// import scalaz._
// import scalaz.Scalaz._

// trait StringImplicits {
//   implicit def stringToStringValidation(str: String) =
//     StringValidation(str.success[Problem])

//   implicit def stringValidationToStringValidation(sv: Validation[Problem,String]) =
//     StringValidation(sv)
// }

// case class StringValidation(val inner: Validation[Problem,String]) extends AkkaDefaults {
//   def trim =
//     StringValidation(inner.map(_.trim))

//   def nonBlank(field: String) =
//     inner.flatMap { str =>
//       if(str.length > 0) {
//         str.success[Problem]
//       } else {
//         Problems.Missing(field).fail
//       }
//     }

//   def lowercase =
//     inner.map(_.toLowerCase)

//   def uppercase =
//     inner.map(_.toLowerCase)

//   def shorterThan(field: String, length: Int) =
//     inner.flatMap(
//       str =>
//         if(str.length < length)
//           str.success[Problem]
//         else
//           Problems.Malformed(field, "tooLong").fail
//     )

//   def longerThan(field: String, length: Int) =
//     inner.flatMap(
//       str =>
//         if(str.length > length)
//           str.success[Problem]
//         else
//           Problems.Malformed(field, "tooShort").fail
//     )

//   def regex(rx: Regex, field: String, description: String) =
//     inner.flatMap { str =>
//       if(rx.findFirstIn(str).isDefined) {
//         str.success[Problem]
//       } else {
//         Problems.Malformed(field, description).fail
//       }
//     }

//   def email(field: String) =
//     regex("^[^@]+@[^@]+$".r, field, "notEmail")

//   def sv: StringValidation =
//     this

//   def fv: FutureValidation[String] =
//     FutureValidation(Promise.successful(inner))
// }
