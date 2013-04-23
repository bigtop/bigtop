// package bigtop.data

// import org.specs2.mutable._
// import scalaz._
// import scalaz.Scalaz._

// class DataValidationSpec extends Specification {
//   "DataValidation" should {
//     "be applicative" in {
//       val actual = (1.success[DataErrors] |@| 2.success[DataErrors]).tupled
//       val expected =  (1, 2).success[DataErrors]
//       actual mustEqual expected
//     }
//   }
// }
