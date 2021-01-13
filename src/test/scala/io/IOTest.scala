package io

import io.IORunLoop.{runAsync, runSync}
import io.IOSyntax._
import org.scalatest.funsuite.AsyncFunSuite

import scala.concurrent.Promise

class IOTest extends AsyncFunSuite {
  test("flatMaps and map run synchronously") {
    val io = for {
      one <- 1.pure
      plusOne <- IO.delay(() => one + 1)
      plusTwo <- (plusOne + 2).pure
    } yield plusTwo + 3

    assert(runSync(io) == 7)
  }

  test("flatMaps and map run asynchronously") {
    val io = for {
      one <- 1.pure
      plusOne <- IO.delay(() => one + 1)
      plusTwo <- (plusOne + 2).pure
    } yield plusTwo + 3

    val resultPromise = Promise[Int]()
    runAsync(io, resultPromise.tryComplete)
    resultPromise.future.map(result => assert(result == 7))
  }

  test("raise error") {
    val io = for {
      one <- 1.pure
      _ <- IO.raiseError[Int](new IllegalArgumentException())
    } yield one + 1

    assertThrows[IllegalArgumentException] { runSync(io) }
  }

  test("recover from error in effect") {
    val io = (for {
      one <- 1.pure
      plusOne <- IO.delay(() => one / 0 + 1)
      plusTwo <- (plusOne + 2).pure
    } yield plusTwo + 3).handleErrorWith(_ => (-1).pure)

    assert(runSync(io) == -1)
  }

  test("recover from error in flatMap handler") {
    val io = 1.pure
      .flatMap(_ => {
        throw new IllegalArgumentException()
        2.pure
      })
      .handleErrorWith(_ => (-1).pure)

    assert(runSync(io) == -1)
  }

  test("no stack overflow") {
    val range = 1 to 10000
    val io = IO.foreach(range) { i => IO.delay(() => i) }

    assert(runSync(io) == range)
  }
}
