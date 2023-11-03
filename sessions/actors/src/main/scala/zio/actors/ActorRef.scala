package zio.actors

import zio._

trait ActorRef[-Message[+Response]] {
  def !(message: Message[Any]): ZIO[Any, Nothing, Unit]                    = send(message)
  def ?[Response](message: Message[Response]): ZIO[Any, Nothing, Response] = ask(message)
  def send(message: Message[Any]): ZIO[Any, Nothing, Unit]
  def ask[Response](message: Message[Response]): ZIO[Any, Nothing, Response]  
}