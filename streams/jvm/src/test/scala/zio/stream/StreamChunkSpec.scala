package zio.stream

import org.scalacheck.Prop.forAll
import org.scalacheck.Test
import utest._
import zio._

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.{ Stream => _ }

object StreamChunkSpec extends TestRuntime with UtestScalacheckExtension with GenIO {

  override val DefaultTimeout = 20.seconds

  override val scalaCheckParams: Test.Parameters = super.scalaCheckParams.withMaxSize(10)

  import ArbitraryStreamChunk._
  import Exit._

  override def tests: Tests = Tests {
    test("map") - propTest {
      forAll { (s: StreamChunk[String, String], f: String => Int) =>
        slurp(s.map(f)) == slurp(s).map(_.map(f))
      }
    }
    test("filter") - propTest {
      forAll { (s: StreamChunk[String, String], p: String => Boolean) =>
        slurp(s.filter(p)) == slurp(s).map(_.filter(p))
      }

    }
    test("filterNot") - propTest {
      forAll { (s: StreamChunk[String, String], p: String => Boolean) =>
        slurp(s.filterNot(p)) == slurp(s).map(_.filterNot(p))
      }
    }
    test("mapConcat") - propTest {
      import ArbitraryChunk._
      forAll { (s: StreamChunk[String, String], f: String => Chunk[Int]) =>
        slurp(s.mapConcat(f)) == slurp(s).map(_.flatMap(v => f(v).toSeq))
      }
    }
    test("dropWhile") - propTest {
      forAll { (s: StreamChunk[String, String], p: String => Boolean) =>
        slurp(s.dropWhile(p)) == slurp(s).map(_.dropWhile(p))
      }
    }
    test("takeWhile") - propTest {
      forAll { (s: StreamChunk[Nothing, String], p: String => Boolean) =>
        val streamTakeWhile = slurp(s.takeWhile(p))
        val listTakeWhile   = slurp(s).map(_.takeWhile(p))
        streamTakeWhile == listTakeWhile
      }
    }
    test("mapAccum") - propTest {
      forAll { s: StreamChunk[String, Int] =>
        val slurped = slurpM(s.mapAccum(0)((acc, el) => (acc + el, acc + el)))
        slurped == slurp(s).map(_.scanLeft(0)((acc, el) => acc + el).drop(1))
      }
    }
    test("mapM") - propTest {
      forAll { (s: StreamChunk[String, Int], f: Int => Int) =>
        slurpM(s.mapM(a => IO.succeed(f(a)))) == slurp(s).map(_.map(f))
      }
    }
    test("++") - propTest {
      forAll { (s1: StreamChunk[String, String], s2: StreamChunk[String, String]) =>
        val listConcat = for {
          left  <- slurp(s1)
          right <- slurp(s2)
        } yield left ++ right
        val streamConcat = slurpM(s1 ++ s2)
        streamConcat == listConcat
      }

    }
    test("zipWithIndex") - propTest {
      forAll((s: StreamChunk[String, String]) => slurp(s.zipWithIndex) == slurp(s).map(_.zipWithIndex))
    }
    test("foreach0") - propTest {
      forAll { (s: StreamChunk[String, Int], cont: Int => Boolean) =>
        var acc = List[Int]()

        val result = unsafeRunSync {
          s.foreachWhile { a =>
            IO.effectTotal {
              if (cont(a)) {
                acc ::= a
                true
              } else false
            }
          }
        }

        result.map(_ => acc.reverse) == slurp(s.takeWhile(cont)).map(_.toList)
      }

    }
    test("foreach") - propTest {
      forAll { s: StreamChunk[String, Int] =>
        var acc = List[Int]()

        val result = unsafeRunSync {
          s.foreach(a => IO.effectTotal(acc ::= a))
        }

        result.map(_ => acc.reverse) == slurp(s).map(_.toList)
      }
    }
    test("monadLaw1") - propTest {
      forAll(
        (x: Int, f: Int => StreamChunk[String, Int]) =>
          slurp(ZStreamChunk.succeedLazy(Chunk(x)).flatMap(f)) == slurp(f(x))
      )
    }
    test("monadLaw2") - propTest {
      forAll(
        (m: StreamChunk[String, Int]) => slurp(m.flatMap(i => ZStreamChunk.succeedLazy(Chunk(i)))) == slurp(m)
      )
    }
    test("monadLaw3") - propTest {
      forAll { (m: StreamChunk[String, Int], f: Int => StreamChunk[String, Int], g: Int => StreamChunk[String, Int]) =>
        val leftStream  = m.flatMap(f).flatMap(g)
        val rightStream = m.flatMap(x => f(x).flatMap(g))
        slurp(leftStream) == slurp(rightStream)
      }

    }
    test("tap") - propTest {
      forAll { (s: StreamChunk[String, String]) =>
        val withoutEffect = slurp(s)
        var acc           = List[String]()
        val tap           = slurp(s.tap(a => IO.effectTotal(acc ::= a)))

        (tap == withoutEffect) &&
        (if (withoutEffect.succeeded) Success(acc.reverse) == withoutEffect else true)
      }

    }
    test("foldLeft") - propTest {
      forAll { (s: StreamChunk[String, String], zero: Int, f: (Int, String) => Int) =>
        unsafeRunSync(s.foldLeft(zero)(f).use(IO.succeed)) == slurp(s).map(_.foldLeft(zero)(f))
      }

    }
    test("fold") - propTest {
      forAll { (s: StreamChunk[Nothing, String], zero: Int, cont: Int => Boolean, f: (Int, String) => Int) =>
        val streamResult = unsafeRunSync(
          s.fold[Any, Nothing, String, Int].flatMap(_(zero, cont, (acc, a) => IO.succeed(f(acc, a)))).use(IO.succeed)
        )
        val listResult = slurp(s).map(l => foldLazyList(l.toList, zero)(cont)(f))
        streamResult == listResult
      }

    }
    test("flattenChunks") - propTest {
      forAll { (s: StreamChunk[String, String]) =>
        val result = unsafeRunSync {
          s.flattenChunks.foldLeft[String, List[String]](Nil)((acc, a) => a :: acc).use(IO.succeed).map(_.reverse)
        }
        result == slurp(s)
      }
    }
  }

  private def slurp[E, A](s: StreamChunk[E, A]): Exit[E, Seq[A]] = s match {
    case s: StreamChunkPure[A] =>
      succeed(
        s.chunks.foldPureLazy(Chunk.empty: Chunk[A])(_ => true)((acc, el) => acc ++ el).toSeq
      )
    case s => slurpM(s)
  }

  private def slurpM[E, A](s: StreamChunk[E, A]): Exit[E, Seq[A]] =
    unsafeRunSync {
      s.foldChunks(Chunk.empty: Chunk[A])(_ => true)((acc, el) => IO.succeed(acc ++ el))
        .use(IO.succeed)
        .map(_.toSeq)
    }

  private def foldLazyList[S, T](list: List[T], zero: S)(cont: S => Boolean)(f: (S, T) => S): S = {
    @tailrec
    def loop(xs: List[T], state: S): S = xs match {
      case head :: tail if cont(state) => loop(tail, f(state, head))
      case _                           => state
    }
    loop(list, zero)
  }
}
