package io

import io.IOFlatMapRewriteRun.runSync
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object IOCallbacksRun {
  private val eventLoop: mutable.Queue[Runnable] = new mutable.Queue[Runnable]()

  def runSync[A](io: IO[A]): Try[A] = {
    var result: Try[Any] = null
    eval(io) { result = _ }

    while (eventLoop.nonEmpty) {
      val task = eventLoop.dequeue()
      task.run()
    }

    result.asInstanceOf[Try[A]]
  }

  private def eval[A, B](io: IO[A])(callback: Try[Any] => Unit): Unit = {
    eventLoop += (() => io match {
      case Return(x) => callback(Success(x))
      case Suspend(x) => callback(Try(x()))
      case FlatMap(m: IO[A], f: (A => IO[B])) => eval(m) {
        case Success(x) => Try(f(x)) match {
          case Success(fx) => eval(fx)(callback)
          case Failure(e) => callback(Failure(e))
        }
        case Failure(e) => callback(Failure(e))
      }
    })
  }
}

class IOCallbacksTest extends AnyFunSuite {
  import IOCallbacksRun.runSync

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

