package io

import org.scalatest.funsuite.AnyFunSuite

sealed trait StackUnsafeIO[A] {
  self =>
  def run: A

  def map[B](f: A => B): StackUnsafeIO[B] =
    new StackUnsafeIO[B] {
      def run: B = f(self.run)
    }

  def flatMap[B](f: A => StackUnsafeIO[B]): StackUnsafeIO[B] =
    new StackUnsafeIO[B] {
      def run: B = f(self.run).run
    }
}

object StackUnsafeIO {
  def unit[A](a: => A): StackUnsafeIO[A] = new StackUnsafeIO[A] {
    def run: A = a
  }

  def flatMap[A, B](fa: StackUnsafeIO[A])(f: A => StackUnsafeIO[B]): StackUnsafeIO[B] = fa flatMap f

  def apply[A](a: => A): StackUnsafeIO[A] = unit(a)

  def forever[A, B](a: StackUnsafeIO[A]): StackUnsafeIO[B] =
    a.flatMap(_ => forever(a))
}

class StackUnsafeIOTest extends AnyFunSuite {
  test("stack overflow") {
    StackUnsafeIO.forever(StackUnsafeIO { println("Waiting...") }).run
  }
}

