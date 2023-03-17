package testing

sealed trait TestResult

object TestResult {
  case object Success extends TestResult
  final case class Failure(inputs: String) extends TestResult
}