package monads

import cats.data.NonEmptyList
import cats.implicits._
import org.scalatest.funsuite.AnyFunSuite

class ResultedSpec extends AnyFunSuite {

  test("Results can be combined") {

    val e1: Either[String, Int] = Right(1)
    val e2: Either[String, Int] = Right(2)
    val e3: Either[String, Int] = Left("request failed")
    val combined1 = e1 |+| e2 |+| e3
    assert(combined1 == Left("request failed"))

    val r1: Result[Int] = Result(1)
    val r2: Result[Int] = Result(2)
    val r3: Result[Int] = Failed("request failed")
    val combined2 = r1 |+| r2 |+| r3
    assert(combined2 == Succeeded(NonEmptyList.of(1, 2)))

  }


}
