package io

object IORunLoop extends IORun {
  override def runSync[A](io: IO[A]): A =
    loop(io, Seq[Any => IO[Any]]()).asInstanceOf[A]

  private def loop(io: IO[Any], stack: Seq[Any => IO[Any]]): Any = io match {
    case Pure(a) => stack match {
      case f +: tail => loop(f(a), tail)
      case _ => a
    }
    case Delay(a) => loop(Pure(a()), stack)
    case FlatMap(m, f) => loop(m, f +: stack)
  }
}
