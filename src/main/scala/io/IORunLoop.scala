package io

import scala.util.{Failure, Success, Try}

object IORunLoop extends IORun {
  override def runSync[A](io: IO[A]): A =
    loop(io, Seq[Any => IO[Any]]()).get.asInstanceOf[A]

  private def loop(io: IO[Any], stack: Seq[Any => IO[Any]]): Try[Any] = io match {
    case Pure(a) => stack match {
      case f +: tail => loop(f(a), tail)
      case _ => Success(a)
    }
    case Delay(a) => loop(Pure(a()), stack)
    case FlatMap(m, f) => loop(m, f +: stack)
    case RaiseError(e) => Failure(e)
  }
}
