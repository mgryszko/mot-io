package io

import org.scalatest.funsuite.AnyFunSuite
import io.IOSyntax._

class IOTest extends AnyFunSuite {
  test("flatMaps and map") {
    val io = for {
      one <- 1.pure
      plusOne <- (one + 1).pure
      plusTwo <- (plusOne + 2).pure
    } yield plusTwo + 3
    assert(IORunLoop.runSync(io) == 7)
  }
}

