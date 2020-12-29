package io3

import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable

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

  val eventLoop: mutable.Queue[Runnable] = new mutable.Queue[Runnable]()

  def eval[A, B](io: IO[A])(callback: Any => Unit): Unit = {
    eventLoop += (() => io match {
      case Return(x) => callback(x)
      case Suspend(x) => callback(x())
      case FlatMap(m: IO[A], f: (A => IO[B])) => eval(m) { x => eval(f(x))(callback) }
    })
  }

  def run[A, B](io: IO[A]): B = {
    var result: Any = null
    eval(io) { result = _ }

    while (eventLoop.nonEmpty) {
      val task = eventLoop.dequeue()
      task.run()
    }

    result.asInstanceOf[B]
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

