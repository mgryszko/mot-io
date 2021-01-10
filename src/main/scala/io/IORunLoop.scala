package io

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object IORunLoop extends IORun {
  sealed trait Bind {
    val isHandler: Boolean
  }
  case class F(f: Any => IO[Any]) extends Bind {
    override val isHandler: Boolean = false

  }
  case class H(h: Throwable => IO[Any]) extends Bind {
    override val isHandler: Boolean = true
  }

  override def runSync[A](io: IO[A]): A =
    loop(io, Seq[Bind]()).get.asInstanceOf[A]

  @tailrec
  private def loop(io: IO[Any], stack: Seq[Bind]): Try[Any] = io match {
    case Pure(a) => stack match {
      case F(f) +: tail => Try(f(a)) match {
        case Success(fa) => loop(fa, tail)
        case Failure(e) => dropUntilHandlerFound(stack) match {
          case H(h) +: tail => loop(h(e), tail)
          case _ => Failure(e)
        }
      }
      case _ => Success(a)
    }
    case Delay(a) => Try(a()) match {
      case Success(aEvaluated) => loop(Pure(aEvaluated), stack)
      case Failure(e) => dropUntilHandlerFound(stack) match {
        case H(h) +: tail => loop(h(e), tail)
        case _ => Failure(e)
      }
    }
    case FlatMap(m, f) => loop(m, F(f) +: stack)
    case RaiseError(e) => Failure(e)
    case HandleError(m, h) => loop(m, H(h) +: stack)
  }

  private def dropUntilHandlerFound(stack: Seq[Bind]) =
    stack.dropWhile(!_.isHandler)
}
