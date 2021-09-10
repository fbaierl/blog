package monads


import cats.Monoid
import cats.data.NonEmptyList

//  implicit val resultInstance: Monad[Result] = new Monad[Result] {
//    override def flatten[B](xs: Result[Result[B]]): Result[B] = ???
//    override def map[A, B](fa: Result[A])(f: A => B): Result[B] =
//      fa match {
//        case Failed => Failed
//        case Succeeded(values) => Succeeded(values map f)
//      }
//    override def flatMap[A, B](fa: Result[A])(f: A => Result[B]): Result[B] = ???
//    override def tailRecM[A, B](a: A)(f: A => Result[Either[A, B]]): Result[B] = ???
//    override def pure[A](x: A): Result[A] = ???
//  }

sealed abstract class ResultInstances {

  implicit def resultInstance[A]: Monoid[Result[A]] = new Monoid[Result[A]] {

    override def empty: Result[A] = Failed()

    override def combine(x: Result[A], y: Result[A]): Result[A] =
      Result.apply(x.extractValues ++ y.extractValues: _*)

  }

}

object Result extends ResultInstances {
  def apply[A](values: A*): Result[A] =
    NonEmptyList.fromList(values.toList).map(nel => Succeeded(nel)).getOrElse(Failed())
}

sealed trait Result[+A] {
  def extractValues: Seq[A] = this match {
    case Succeeded(xs) => xs.toList
    case _ => Nil
  }
}
case class Succeeded[A](values: NonEmptyList[A]) extends Result[A]
case class Failed(reason: String = "failure") extends Result[Nothing]




