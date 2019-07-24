package zio

import org.scalacheck.{ Prop, Test }
import org.scalacheck.util.Pretty
import utest._

import scala.reflect.ClassTag

/*
  https://github.com/lihaoyi/utest/issues/2#issuecomment-67300735
 */
trait UtestScalacheckExtension {

  protected[this] object UTestReporter extends Test.TestCallback {
    private val prettyParams = Pretty.defaultParams

    override def onTestResult(name: String, res: org.scalacheck.Test.Result) = {
      val scalaCheckResult = if (res.passed) "" else Pretty.pretty(res, prettyParams)
      assert(scalaCheckResult.isEmpty)
    }
  }

  def scalaCheckParams: Test.Parameters = Test.Parameters.default

  def propTest(prop: Prop): Unit = prop.check(scalaCheckParams.withTestCallback(UTestReporter))

  def assertTuple[A, B](a: (A, B), b: (A, B)): Unit =
    assert(a._1 == b._1, a._2 == b._2)

  //TODO: What are methods using this really testing?
  def interceptNot[T: ClassTag](expr: => Unit): Unit =
    try {
      expr
      assert(true)
    } catch {
      case _: T         => assert(false) //Find a way to have actual error here
      case _: Throwable => assert(true)
    }

  implicit class MustEqual[A](a: A) {
    @inline def must_===(b: A): Unit = assert(a == b)
  }

}
