package io

import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.tailrec

sealed trait Step
case class Done[A](x: A) extends Step
case class Eval[A](m: IO[A]) extends Step
case class Continue[A, B](f: A => IO[B]) extends Step

object IOStackRun {
  private def eval[A, B](io: IO[A]): List[Step] = {
    io match {
      case Return(x) => Done(x) :: Nil
      case Suspend(x) => Done(x()) :: Nil
      case FlatMap(m: IO[A], f: (A => IO[B])) => Eval(m) :: Continue(f) :: Nil
    }
  }

  @tailrec
  private def process[A, B](eventLoop: List[Step]): A = {
    eventLoop match {
      case Done(x: A) :: Nil => x
      case Done(x: A) :: Continue(f: (A => IO[B])) :: rest => process(Eval(f(x)) :: rest)
      case Eval(m) :: rest => process(eval(m) ++ rest)
    }
  }

  def runSync[A](io: IO[A]): A = {
    process(eval(io)).asInstanceOf[A]
  }
}

class IOStackTest extends AnyFunSuite {
  import IOStackRun.runSync

  test("a couple of flatMaps") {
    val io = IO(1)
      .flatMap(x =>
        IO(x + 1)
      )
      .flatMap(x =>
        IO(x + 2)
      )
      .flatMap(x =>
        IO(x + 3)
      )
    assert(runSync(io) == 7)
  }

  test("stack overflow") {
    runSync(IO.forever(IO.effect(() => println("Waiting..."))))
  }
}

