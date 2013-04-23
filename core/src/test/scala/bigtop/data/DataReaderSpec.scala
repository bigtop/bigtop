// package bigtop.data

// import org.specs2.mutable._
// import scalaz._
// import scalaz.Scalaz._

// class DataReaderSpec extends Specification {
//   implicit object StringToInt extends DataReader[String, Int] {
//     def read(in: String) =
//       try {
//         in.toInt.success[DataErrors]
//       } catch {
//         case exn: NumberFormatException =>
//           DataErrors.Malformed(
//             DataPath.Empty,
//             "Expected an integer, received " + in
//           ).fail[Int]
//       }
//   }

//   "DataReader" should {
//     "be applicative" in {
//       implicit val reader = StringToInt |@| StringToInt
//       val actual = reader(("1", "1"))
//       val expected = Success((1, 1))
//       actual mustEqual expected
//     }
//   }
// }
