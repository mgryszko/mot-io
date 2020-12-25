package io

import org.scalatest.funsuite.AnyFunSuite

sealed trait IO[A] {
  self =>
  def run: A

  def map[B](f: A => B): IO[B] =
    new IO[B] {
      def run: B = f(self.run)
    }

  def flatMap[B](f: A => IO[B]): IO[B] =
    new IO[B] {
      def run: B = f(self.run).run
    }
}

object IO {
  def unit[A](a: => A): IO[A] = new IO[A] {
    def run: A = a
  }

  def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f

  def apply[A](a: => A): IO[A] = unit(a)

  def forever[A, B](a: IO[A]): IO[B] =
    a.flatMap(_ => forever(a))
}

class IOTest extends AnyFunSuite {
  test("stack overflow") {
    IO.forever(IO { println("Waiting...") }).run
  }
}

