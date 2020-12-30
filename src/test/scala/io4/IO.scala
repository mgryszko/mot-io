package io4

import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

sealed trait IO[A] {
  def map[B](f: A => B): IO[B] = flatMap(f andThen (Return(_)))

  def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)
}

case class Return[A](x: A) extends IO[A]
case class Suspend[A](x: () => A) extends IO[A]
case class FlatMap[A, B](m: IO[A], f: A => IO[B]) extends IO[B]

sealed trait Step
case class Done[A](x: A) extends Step
case class Eval[A](m: IO[A]) extends Step
case class Continue[A, B](f: A => IO[B]) extends Step

object IO {
  def apply[A](a: => A): IO[A] = Return(a)

  def forever[A, B](a: IO[A]): IO[B] =
    a.flatMap(_ => forever(a))

  def eval[A, B](io: IO[A]): List[Step] = {
    io match {
      case Return(x) => Done(x) :: Nil
      case Suspend(x) => Done(x()) :: Nil
      case FlatMap(m: IO[A], f: (A => IO[B])) => Eval(m) :: Continue(f) :: Nil
    }
  }

  @tailrec
  def process[A, B](eventLoop: List[Step]): A = {
    eventLoop match {
      case Done(x: A) :: Nil => x
      case Done(x: A) :: Continue(f: (A => IO[B])) :: rest => process(Eval(f(x)) :: rest)
      case Eval(m) :: rest => process(eval(m) ++ rest)
    }
  }

  def run[A, B](io: IO[A]): B = {
    process(eval(io)).asInstanceOf[B]
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

