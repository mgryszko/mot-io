package io

sealed trait IO[A] {
  def map[B](f: A => B): IO[B] = flatMap(f andThen (Return(_)))

  def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)
}

case class Return[A](x: A) extends IO[A]
case class Suspend[A](x: () => A) extends IO[A]
case class FlatMap[A, B](m: IO[A], f: A => IO[B]) extends IO[B]

object IO {
  def apply[A](a: => A): IO[A] = Return(a)

  def effect[A](a: () => A): IO[A] = Suspend(a)

  def forever[A, B](a: IO[A]): IO[B] =
    a.flatMap(_ => forever(a))
}
