package common

import scala.concurrent._
import ExecutionContext.Implicits.global

object Utils {
  def isEmpty(str: String): Boolean = str == null || str.trim().isEmpty()
  def trim(str: String): String = if (str != null) str.trim() else null
  def toOpt(str: String): Option[String] = if (isEmpty(str)) None else Some(trim(str))

  def transform[T, U](pair: (Option[T], Option[U])): Option[(T, U)] = for (a <- pair._1; b <- pair._2) yield (a, b)
  def transform[T, U](pair: (Future[T], Future[U])): Future[(T, U)] = for (a <- pair._1; b <- pair._2) yield (a, b)
  def transform[A](o: Option[Future[A]]): Future[Option[A]] = o.map(f => f.map(Option(_))).getOrElse(Future.successful(None))
}
