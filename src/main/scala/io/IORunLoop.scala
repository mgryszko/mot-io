package io

import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Promise}
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

  override def runSync[A](io: IO[A]): A = {
    val resultPromise = Promise[A]()
    runAsync(io, resultPromise.tryComplete)
    Await.result(resultPromise.future, Duration.Inf)
  }

  override def runAsync[A](io: IO[A], cb: Try[A] => Unit) =
    loop(io, Seq[Bind](), cb.asInstanceOf[Try[Any] => Unit])

  private def loop(io: IO[Any], stack: Seq[Bind], cb: Try[Any] => Unit): Unit = io match {
    case Pure(a) => stack match {
      case F(f) +: tail => Try(f(a)) match {
        case Success(fa) => loop(fa, tail, cb)
        case Failure(e) => dropUntilHandlerFound(stack) match {
          case H(h) +: tail => loop(h(e), tail, cb)
          case _ => cb(Failure(e))
        }
      }
      case _ => cb(Success(a))
    }
    case Delay(a) => Try(a()) match {
      case Success(aEvaluated) => loop(Pure(aEvaluated), stack, cb)
      case Failure(e) => dropUntilHandlerFound(stack) match {
        case H(h) +: tail => loop(h(e), tail, cb)
        case _ => cb(Failure(e))
      }
    }
    case FlatMap(m, f) => loop(m, F(f) +: stack, cb)
    case RaiseError(e) => dropUntilHandlerFound(stack) match {
      case H(h) +: tail => loop(h(e), tail, cb)
      case _ => cb(Failure(e))
    }
    case HandleError(m, h) => loop(m, H(h) +: stack, cb)
    case Async(asyncCb) => asyncCb(a => loop(a.fold(RaiseError(_), Pure(_)), stack, cb))
  }

  private def dropUntilHandlerFound(stack: Seq[Bind]) =
    stack.dropWhile(!_.isHandler)
}
