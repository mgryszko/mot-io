package io

sealed trait IO[A] {
  def map[B](f: A => B): IO[B] = flatMap(f andThen (Return(_)))

  def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)

  def recover(f: Throwable => IO[A]): IO[A] = Recover(this, f)
}

case class Return[A](x: A) extends IO[A]
case class Suspend[A](x: () => A) extends IO[A]
case class FlatMap[A, B](m: IO[A], f: A => IO[B]) extends IO[B]
case class Recover[A](m: IO[A], f: Throwable => IO[A]) extends IO[A]
case class Fail[A](e: Throwable) extends IO[A]

object IO {
  def apply[A](a: => A): IO[A] = Return(a)

  def effect[A](a: () => A): IO[A] = Suspend(a)

  def fail[A](e: Throwable): IO[A] = Fail(e)

  def forever[A, B](a: IO[A]): IO[B] =
    a.flatMap(_ => forever(a))
}
