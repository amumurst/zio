package zio

import utest._
import org.scalacheck.Prop.forAll

object ExitSpec extends TestSuite with UtestScalacheckExtension {
  import Cause._
  import ArbitraryCause._

  override def tests: Tests = Tests {
    test("Cause") {
      test("`Cause#died` and `Cause#stripFailures` are consistent") - propTest(e1)
      test("`Cause.equals` is symmetric") - propTest(e2)
      test("`Cause.equals` and `Cause.hashCode` satisfy the contract") - propTest(e3)
    }
    test("Then") {
      test("`Then.equals` satisfies associativity") - propTest(e4)
      test("`Then.equals` satisfies distributivity") - propTest(e5)
    }
    test("Both") {
      test("`Both.equals` satisfies associativity") - propTest(e6)
      test("`Both.equals` satisfies commutativity") - propTest(e7)
    }
  }

  private def e1() = forAll { c: Cause[String] =>
    if (c.died) c.stripFailures.isDefined
    else c.stripFailures.isEmpty
  }

  private def e2() = forAll { (a: Cause[String], b: Cause[String]) =>
    (a == b) == (b == a)
  }

  private def e3() =
    forAll { (a: Cause[String], b: Cause[String]) =>
      if (a == b) a.hashCode == b.hashCode else true
    } //.set(minTestsOk = 10, maxDiscardRatio = 99.0f)

  private def e4() = forAll { (a: Cause[String], b: Cause[String], c: Cause[String]) =>
    (Then(Then(a, b), c) == Then(a, Then(b, c))) &&
    (Then(a, Then(b, c)) == Then(Then(a, b), c))
  }

  private def e5() = forAll { (a: Cause[String], b: Cause[String], c: Cause[String]) =>
    (Then(a, Both(b, c)) == Both(Then(a, b), Then(a, c))) &&
    (Then(Both(a, b), c) == Both(Then(a, c), Then(b, c)))
  }

  private def e6() = forAll { (a: Cause[String], b: Cause[String], c: Cause[String]) =>
    (Both(Both(a, b), c) == Both(a, Both(b, c))) &&
    (Both(Both(a, b), c) == Both(a, Both(b, c)))
  }

  private def e7() = forAll { (a: Cause[String], b: Cause[String]) =>
    Both(a, b) == Both(b, a)
  }
}
