package stm

trait Journal {

  def addToDo(transactionId: TransactionId, todo: ToDo): Unit

  def analyze: AnalysisResult

  def commit(): Unit

  def collectToDos(): Map[TransactionId, ToDo]

  def get[A](tRef: TRef[A]): Option[Entry]

  def getOrElseMake[A](tRef: TRef[A]): Entry

  def isValid: Boolean

  def put[A](tRef: TRef[A], entry: Entry): Unit
}

object Journal {

  def make(): Journal =
    new Journal {
      val map = scala.collection.mutable.Map.empty[TRef[_], Entry]

      def addToDo(transactionId: TransactionId, todo: ToDo): Unit = {
        val iterator = map.iterator
        while (iterator.hasNext) {
          val (tRef, _) = iterator.next()
          var loop = true
          while (loop) {
            val oldTodos = tRef.todos.get
            if (!oldTodos.contains(transactionId)) {
              val newTodos = oldTodos + (transactionId -> todo)
              loop = !tRef.todos.compareAndSet(oldTodos, newTodos)
            } else {
              loop = false
            }
          }
        }
      }
      
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

      def collectToDos(): Map[TransactionId, ToDo] = {
        val iterator = map.iterator
        val todos = scala.collection.mutable.Map.empty[TransactionId, ToDo]
        while (iterator.hasNext) {
          val (tref, _) = iterator.next()
          val todo = tref.todos.getAndSet(Map.empty)
          todos ++= todo
        }
        todos.toMap
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