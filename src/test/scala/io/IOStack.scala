package io

import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

sealed trait Step
case class Done[A](x: Try[A]) extends Step
case class Eval[A](m: IO[A]) extends Step
case class Continue[A, B](f: A => IO[B]) extends Step

object IOStackRun {
  private def eval[A, B](io: IO[A]): List[Step] = {
    io match {
      case Return(x) => Done(Success(x)) :: Nil
      case Suspend(x) => Done(Try(x())) :: Nil
      case FlatMap(m: IO[A], f: (A => IO[B])) => Eval(m) :: Continue(f) :: Nil
    }
  }

  @tailrec
  private def process[A, B](eventLoop: List[Step]): Try[A] = {
    eventLoop match {
      case Done(res: Try[A]) :: Nil => res
      case Done(res: Try[A]) :: Continue(f: (A => IO[B])) :: rest => res match {
        case Success(x) => Try(f(x)) match {
          case Success(fx) => process(Eval(fx) :: rest)
          case Failure(e) => Failure(e)
        }
        case Failure(e) => Failure(e)
      }
      case Eval(m) :: rest => process(eval(m) ++ rest)
    }
  }

  def runSync[A](io: IO[A]): Try[A] = process(eval(io))
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
    assert(runSync(io) == Success(7))
  }

  test("error") {
    val exception = new IllegalArgumentException("bang!")

    val returnFlatmapIo = IO(1).flatMap(_ => throw exception)
    assert(runSync(returnFlatmapIo) == Failure(exception))

    val suspendFlatmapIo = IO.effect { () => 1 }.flatMap(_ => throw exception)
    assert(runSync(suspendFlatmapIo) == Failure(exception))

    val suspendFlatmapIo2 = IO.effect { () => throw exception; 1 }.flatMap(x => IO(x + 2))
    assert(runSync(suspendFlatmapIo2) == Failure(exception))

    val multiFlatmapIo = IO(1).flatMap { x => throw exception; IO(x + 1) }.flatMap(x => IO(x + 2))
    assert(runSync(multiFlatmapIo) == Failure(exception))

    val multiFlatmapIo2 = IO(1).flatMap(x => IO(x + 1)).flatMap { x => throw exception; IO(x + 2) }
    assert(runSync(multiFlatmapIo2) == Failure(exception))
  }

  test("stack overflow") {
    runSync(IO.forever(IO.effect(() => println("Waiting..."))))
  }
}

