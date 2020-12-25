package io2

import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.tailrec

sealed trait IO[A] {
  def map[B](f: A => B): IO[B] = flatMap(f andThen (Return(_)))

  def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)
}

case class Return[A](x: A) extends IO[A]
case class Suspend[A](x: () => A) extends IO[A]
case class FlatMap[A, B](m: IO[A], f: A => IO[B]) extends IO[B]

object IO {
  def apply[A](a: => A): IO[A] = Return(a)

  def forever[A, B](a: IO[A]): IO[B] =
    a.flatMap(_ => forever(a))

  @tailrec
  def run[A](io: IO[A]): A = io match {
    case Return(x) => x
    case Suspend(x) => x()
    case FlatMap(outer, g) => outer match {
      case Return(y) => run(g(y))
      case Suspend(y) => run(g(y()))
      case FlatMap(inner, f) => run(inner flatMap(x => f(x) flatMap g))
    }
  }
}

class IOTest extends AnyFunSuite {
  test("a couple of flatMaps") {
    println(
      IO.run(
        IO(1)
          .flatMap(x =>
            IO(x + 1)
          )
          .flatMap(x =>
            IO(x + 2)
          )
          .flatMap(x =>
            IO(x + 3)
          )
      )
    )
  }

  test("stack overflow") {
    IO.run(IO.forever(Suspend(() => println("Waiting..."))))
  }
}

