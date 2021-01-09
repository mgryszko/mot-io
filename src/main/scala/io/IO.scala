package io

sealed trait IO[+A] {
  def map[B](f: A => B): IO[B] = flatMap(f andThen (Pure(_)))

  def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)
}

case class Pure[+A](a: A) extends IO[A]

case class Delay[+A](a: () => A) extends IO[A]

case class FlatMap[A, B](io: IO[A], f: A => IO[B]) extends IO[B]

case class RaiseError[A](e: Throwable) extends IO[A]

trait IORun {
  def runSync[A](io: IO[A]): A
}

object IO {
  def apply[A](a: => A): IO[A] = Pure(a)

  def delay[A](a: () => A): IO[A] = Delay(a)

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
