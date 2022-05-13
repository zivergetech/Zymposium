package stm

trait Journal {

  def analyze: AnalysisResult

  def commit(): Unit

  def get[A](tRef: TRef[A]): Option[Entry]

  def getOrElseMake[A](tRef: TRef[A]): Entry

  def isValid: Boolean

  def put[A](tRef: TRef[A], entry: Entry): Unit
}

object Journal {

  def make(): Journal =
    new Journal {
      val map = scala.collection.mutable.Map.empty[TRef[_], Entry]
      
      def analyze: AnalysisResult = {
        val iterator = map.iterator
        var loop = true
        var result: AnalysisResult = AnalysisResult.ReadOnly
        while (iterator.hasNext && loop) {
          val (_, entry) = iterator.next()
          if (entry.isInvalid) {
            result = AnalysisResult.Invalid
            loop = false
          } else if (entry.isChanged) {
            result = AnalysisResult.ReadWrite
          }
        }
        result
      }

      def commit(): Unit =
        map.foreach { case (_, entry) => entry.commit() }

      def get[A](tRef: TRef[A]): Option[Entry] =
        map.get(tRef)
      def getOrElseMake[A](tRef: TRef[A]): Entry =
        map.getOrElseUpdate(tRef, Entry.make(tRef))
      def isValid: Boolean =
        analyze != AnalysisResult.Invalid
      def put[A](tRef: TRef[A], entry: Entry): Unit =
        map.put(tRef, entry)
    }
}