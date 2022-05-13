package stm

sealed trait Entry {
  type S

  val tRef: TRef[S]

  def versioned: Versioned[S]

  var newValue: S

  var isChanged: Boolean =
    false

  def isInvalid: Boolean =
    !isValid

  def isValid: Boolean =
    versioned eq tRef.versioned

  def unsafeGet[A]: A =
    newValue.asInstanceOf[A]

  def unsafeSet[A](a: A): Unit = {
    newValue = a.asInstanceOf[S]
    isChanged = true
  }

  def commit(): Unit = {
    tRef.versioned = new Versioned(newValue)
  }
}

object Entry {

  def make[A](tRef0: TRef[A]): Entry = {
    val versioned0 = tRef0.versioned
    new Entry {
      type S = A
      var newValue = versioned0.value
      val tRef: TRef[A] = tRef0
      def versioned: Versioned[A] = versioned0
    }
  }
}