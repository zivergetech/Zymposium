package stm

import java.util.concurrent.atomic.AtomicReference

trait TRef[A] { self =>

  @volatile var versioned: Versioned[A]

  val todos: AtomicReference[Map[TransactionId, ToDo]]

  def get: STM[A] =
    STM.Transaction { journal =>
      val entry = journal.getOrElseMake(self)
      entry.unsafeGet[A]
    }

  def set(a: A): STM[Unit] =
    STM.Transaction { journal =>
      val entry = journal.getOrElseMake(self)
      entry.unsafeSet(a)  
    }
}

object TRef {

  def make[A](a: A): STM[TRef[A]] =
    STM.Transaction { journal =>
      val versioned0 = new Versioned(a)
      val tRef = new TRef[A] {
        @volatile var versioned = versioned0
        val todos = new AtomicReference(Map.empty[TransactionId, ToDo])
      }
      val entry = Entry.make(tRef)
      journal.put(tRef, entry)
      tRef
    }
}