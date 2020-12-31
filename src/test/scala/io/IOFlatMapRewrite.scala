package io

import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object IOFlatMapRewriteRun {
  @tailrec
  def runSync[A](io: IO[A]): Try[A] = io match {
    case Return(x) => Success(x)
    case Suspend(x) => Try(x())
    case FlatMap(outer, g) => outer match {
      case Return(y) => Try(g(y)) match {
        case Success(res) => runSync(res)
        case Failure(e) => Failure(e)
      }
      case Suspend(y) => Try(g(y())) match {
        case Success(res) => runSync(res)
        case Failure(e) => Failure(e)
      }
      case FlatMap(inner, f) => runSync(inner flatMap(x => f(x) flatMap g))
    }
  }
}

class IOFlatMapRewriteTest extends AnyFunSuite {
  import IOFlatMapRewriteRun.runSync

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

  test("no stack overflow") {
    runSync(IO.forever(IO.effect { () => println("Waiting...") }))
  }
}

