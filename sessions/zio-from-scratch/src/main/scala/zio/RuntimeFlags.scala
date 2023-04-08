package zio

final case class RuntimeFlags(set: Set[RuntimeFlag]) {
  def contains(flag: RuntimeFlag): Boolean = set.contains(flag)

  def isInterruptible: Boolean = set.contains(RuntimeFlag.Interruptible)

  def +(runtimeFlag: RuntimeFlag) =
    copy(set = set + runtimeFlag)

  def -(runtimeFlag: RuntimeFlag) =
    copy(set = set - runtimeFlag)
}

object RuntimeFlags {
  val default: RuntimeFlags = RuntimeFlags(Set(RuntimeFlag.Interruptible))
}