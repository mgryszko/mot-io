package io

import io.IORunLoop.runSync
import org.scalatest.funsuite.AnyFunSuite
import io.IOSyntax._

class IOTest extends AnyFunSuite {
  test("flatMaps and map") {
    val io = for {
      one <- 1.pure
      plusOne <- IO.delay(() => one + 1)
      plusTwo <- (plusOne + 2).pure
    } yield plusTwo + 3

    assert(runSync(io) == 7)
  }

  test("no stack overflow") {
    val range = 1 to 10000
    val io = IO.foreach(range) { i => IO.delay(() => i) }

    assert(runSync(io) == range)
  }
}

