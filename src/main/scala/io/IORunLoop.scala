package io

object IORunLoop extends IORun {
  override def runSync[A](io: IO[A]): A =
    loop(io)

  private def loop[A](io: IO[A]): A = io match {
    case Pure(a) => a
    case Delay(a) => a()
    case FlatMap(m, f) => loop(f(loop(m)))
  }
}
