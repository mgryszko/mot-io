package io

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

sealed trait IO[+A] {
  def map[B](f: A => B): IO[B] = flatMap(f andThen (Pure(_)))

  def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)

  def *> [B](m: IO[B]): IO[B] = flatMap(_ => m)

  def handleErrorWith[B](handler: Throwable => IO[B]): IO[B] = HandleError(this, handler)

  def fork(): IO[Fiber[A]] = Fork(this)
}

case class Pure[A](a: A) extends IO[A]

case class Delay[A](a: () => A) extends IO[A]

case class FlatMap[A, B](io: IO[A], f: A => IO[B]) extends IO[B]

case class RaiseError[A](e: Throwable) extends IO[A]

case class HandleError[A, B](io: IO[A], handler: Throwable => IO[B]) extends IO[B]

case class Async[A](cb: (Try[A] => Unit) => Unit) extends IO[A]

case class Shift(ec: ExecutionContext) extends IO[Unit]

case class Fork[A](io: IO[A]) extends IO[Fiber[A]]

case class Join[A](fiber: Fiber[A]) extends IO[A]

trait Fiber[+A] {
  def join: IO[A] = Join(this)
}

trait IORun {
  def runSync[A](io: IO[A]): A

  def runAsync[A](io: IO[A], cb: Try[A] => Unit)
}

object IO {
  def apply[A](a: => A): IO[A] = Pure(a)

  def delay[A](a: () => A): IO[A] = Delay(a)

  def fromFuture[A](ft: Future[A])(implicit ec: ExecutionContext): IO[A] = async(ft.onComplete)

  def async[A](cb: (Try[A] => Unit) => Unit): IO[A] = Async(cb)

  def shift(implicit ec: ExecutionContext): IO[Unit] = Shift(ec)

  def foreach[A, B](seq: Iterable[A])(f: A => IO[B]): IO[Iterable[B]] =
    seq.foldLeft(IO(Vector.empty[B])) { (acc, a) =>
      for {
        results <- acc
        result <- f(a)
      } yield results :+ result
    }

  def raiseError[A](e: Throwable): IO[A] = RaiseError(e)

  def forever[A, B](a: IO[A]): IO[B] =
    a.flatMap(_ => forever(a))
}

object IOSyntax {
  implicit class IOOps[A](value: A) {
    def pure: IO[A] = IO(value)
  }
}
