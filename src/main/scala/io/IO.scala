package io

sealed trait IO[A] {
  def map[B](f: A => B): IO[B] = flatMap(f andThen (Pure(_)))

  def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)
}

case class Pure[A](a: A) extends IO[A]

case class Delay[A](a: () => A) extends IO[A]

case class FlatMap[A, B](io: IO[A], f: A => IO[B]) extends IO[B]

trait IORun {
  def runSync[A](io: IO[A]): A
}

object IO {
  def apply[A](a: => A): IO[A] = Pure(a)

  def delay[A](a: () => A): IO[A] = Delay(a)

  def forever[A, B](a: IO[A]): IO[B] =
    a.flatMap(_ => forever(a))
}

object IOSyntax {
  implicit class IOOps[A](value: A) {
    def pure: IO[A] = IO(value)
  }
}
