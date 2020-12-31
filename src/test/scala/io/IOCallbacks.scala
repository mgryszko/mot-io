package io

import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable

object IOCallbacksRun {
  private val eventLoop: mutable.Queue[Runnable] = new mutable.Queue[Runnable]()

  def runSync[A](io: IO[A]): A = {
    var result: Any = null
    eval(io) { result = _ }

    while (eventLoop.nonEmpty) {
      val task = eventLoop.dequeue()
      task.run()
    }

    result.asInstanceOf[A]
  }

  private def eval[A, B](io: IO[A])(callback: Any => Unit): Unit = {
    eventLoop += (() => io match {
      case Return(x) => callback(x)
      case Suspend(x) => callback(x())
      case FlatMap(m: IO[A], f: (A => IO[B])) => eval(m) { x => eval(f(x))(callback) }
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
    assert(runSync(io) == 7)
  }

  test("no stack overflow") {
    runSync(IO.forever(IO.effect { () => println("Waiting...") }))
  }
}

