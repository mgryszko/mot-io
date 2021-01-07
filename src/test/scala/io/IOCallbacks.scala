package io

import io.IOCallbacksRun.runAsyncToFuture
import org.scalatest.funsuite.AnyFunSuite

import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, Promise}
import scala.util.{Failure, Success, Try}

object IOCallbacksRun {
  private val executor: ExecutorService = Executors.newFixedThreadPool(10)

  def runSync[A](io: IO[A]): Try[A] =
    Await.ready(runAsyncToFuture(io), Duration.Inf).value.get

  def runAsync[A](io: IO[A])(callback: Try[A] => Unit) {
    eval(io)(callback.asInstanceOf[Try[Any] => Unit])
  }

  def runAsyncToFuture[A](io: IO[A]): Future[A] = {
    val result = Promise[Any]()
    eval(io)(result.tryComplete)
    result.future.asInstanceOf[Future[A]]
  }

  private def eval[A, B](io: IO[A])(callback: Try[Any] => Unit): Unit =
    executor.submit(new Runnable {
      override def run() {
        io match {
          case Return(x) => callback(Success(x))
          case Suspend(x) => callback(Try(x()))
          case FlatMap(m: IO[A], f: (A => IO[B])) => eval(m) {
            case Success(x) => Try(f(x)) match {
              case Success(fx) => eval(fx)(callback)
              case Failure(e) => callback(Failure(e))
            }
            case Failure(e) => callback(Failure(e))
          }
          case Fail(e) => callback(Failure(e))
          case Recover(m: IO[A], f: (Throwable => IO[A])) => eval(m) {
            case Success(x) => callback(Success(x))
            case Failure(e) => Try(f(e)) match {
              case Success(x) => eval(x)(callback)
              case Failure(ee) => callback(Failure(ee))
            }
          }
        }
      }
    })
}

class IOCallbacksTest extends AnyFunSuite {
  import IOCallbacksRun.runSync

  val exception = new IllegalArgumentException("bang!")

  test("a couple of flatMaps, sync execution") {
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

  test("a couple of flatMaps, async execution") {
    val io = for {
      one <- IO(1)
      two <- IO(one + 1)
      three <- IO(two + 2)
    } yield three + 3
    assert(Await.result(runAsyncToFuture(io), Duration.Inf) == 7)
  }

  test("error") {
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

  test("fail and recover") {
    val failedIo = for {
      one <- IO(1)
      two <- IO(one + 1)
      _ <- IO.fail[Int](exception)
      three <- IO(two + 2)
    } yield three
    val recoveredIo = failedIo.recover(e => IO(-1))

    assert(runSync(failedIo) == Failure(exception))
    assert(runSync(recoveredIo) == Success(-1))
  }

  test("recovery not required") {
    val io = for {
      one <- IO(1)
      two <- IO(one + 1)
    } yield two
    val recoveredIo = io.recover(e => IO(-1))

    assert(runSync(recoveredIo) == Success(2))
  }

  test("failure in recovery") {
    val failedIo = IO.fail[Int](exception)
    val recoveredIo = failedIo.recover { _ => throw exception; IO(-1) }

    assert(runSync(recoveredIo) == Failure(exception))
  }

  test("no stack overflow") {
    runSync(IO.forever(IO.effect { () => println("Waiting...") }))
  }
}

