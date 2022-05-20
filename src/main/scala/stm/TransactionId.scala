package stm

import java.util.concurrent.atomic.AtomicLong

object TransactionId {

  private val transactionIdCounter: AtomicLong =
    new AtomicLong(0L)

  def make(): TransactionId =
    transactionIdCounter.getAndIncrement()
}