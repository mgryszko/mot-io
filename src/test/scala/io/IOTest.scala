package io

import io.IORunLoop.{runAsync, runSync}
import io.IOSyntax._
import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, Promise}

class IOTest extends AnyFunSuite {
  implicit val ec: ExecutionContext = ExecutionContext.global

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
    assert(Await.result(resultPromise.future, Duration.Inf) == 7)
  }

  test("no stack overflow") {
    val range = 1 to 10000
    val io = IO.foreach(range) { i => IO.delay(() => i) }

    assert(runSync(io) == range)
  }

  test("thread shift") {
    val io = for {
      one <- 1.pure
      _ <- IO.shift
      plusOne <- IO.delay(() => one + 1)
      _ <- IO.shift
      plusTwo <- (plusOne + 2).pure
    } yield plusTwo + 3

    assert(runSync(io) == 7)
  }

  test("async") {
    val io = for {
      one <- IO.fromFuture(Future(1))
      plusOne <- IO.delay(() => one + 1)
      plusTwo <- (plusOne + 2).pure
    } yield plusTwo + 3

    val resultPromise = Promise[Int]()
    runAsync(io, resultPromise.tryComplete)
    resultPromise.future.map(result => assert(result == 7))
  }

  test("raise error, don't recover") {
    val io = for {
      one <- 1.pure
      _ <- IO.raiseError[Int](new IllegalArgumentException())
    } yield one + 1

    assertThrows[IllegalArgumentException] { runSync(io) }
  }

  test("raise error and recover") {
    val io = IO.raiseError[Int](new IllegalArgumentException())
      .handleErrorWith(_ => (-1).pure)

    assert(runSync(io) == -1)
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

  test("recover from error in asynchronous computation") {
    val io = IO.fromFuture(Future.failed(new IllegalArgumentException()))
      .handleErrorWith(_ => (-1).pure)

    assert(runSync(io) == -1)
  }
}
