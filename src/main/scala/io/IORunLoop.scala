package io

object IORunLoop extends IORun {
  override def runSync[A](io: IO[A]): A = ???
}
