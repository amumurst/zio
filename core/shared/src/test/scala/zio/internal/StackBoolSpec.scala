package zio.internal

import org.scalacheck.{ Arbitrary, Gen, Prop }
import utest._
import zio.UtestScalacheckExtension

object StackBoolSpec extends TestSuite with UtestScalacheckExtension {

  override def tests: Tests = Tests {
    test("Size tracking                 ") - propTest(e0)
    test("From/to list identity         ") - propTest(e1)
    test("Push/pop example              ") - propTest(e2)
    test("Peek/pop identity             ") - propTest(e3)
    test("GetOrElse index out of bounds ") - e4
  }
  import Arbitrary._

  private val generator: Gen[List[Boolean]] = boolListGen(0, 200)

  def e0() =
    Prop.forAll(generator) { list: List[Boolean] =>
      StackBool(list: _*).size == list.length
    }

  def e1() =
    Prop.forAll(generator) { list: List[Boolean] =>
      StackBool(list: _*).toList == list
    }

  def e2() =
    Prop.forAll(generator) { list: List[Boolean] =>
      val stack = StackBool()

      list.foreach(stack.push(_))

      list.reverse.foldLeft(true) {
        case (result, flag) =>
          result && (stack.popOrElse(!flag) == flag)
      }
    }

  def e3() =
    Prop.forAll(generator) { list: List[Boolean] =>
      val stack = StackBool()

      list.foreach(stack.push(_))

      list.reverse.foldLeft(true) {
        case (result, flag) =>
          val peeked = stack.peekOrElse(!flag)
          val popped = stack.popOrElse(!flag)

          result && (peeked == popped)
      }
    }

  def e4() = {
    val stack  = StackBool()
    val result = stack.getOrElse(100, true)
    assert(result)
  }

  private def boolListGen(min: Int, max: Int) =
    for {
      size <- Gen.choose(min, max)
      g    <- Gen.listOfN(size, Arbitrary.arbitrary[Boolean])
    } yield g
}
