package stm

sealed trait AnalysisResult

object AnalysisResult {
  case object Invalid extends AnalysisResult
  case object ReadOnly extends AnalysisResult
  case object ReadWrite extends AnalysisResult
}