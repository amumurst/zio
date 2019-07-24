package zio.stream

import java.util.concurrent.TimeUnit

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import utest.{ test, Tests }
import zio._
import zio.clock.Clock
import zio.duration._
import zio.stream.ArbitraryStream._
import zio.stream.ZSink.Step
import zio.testkit.TestClock

import scala.{ Stream => _ }

object SinkSpec extends TestRuntime with StreamTestUtils with UtestScalacheckExtension with GenIO {

  override def tests: Tests = Tests {
    test("Constructors") {
      test("Sink") {
        test("foldLeft") - propTest {
          forAll { (s: Stream[String, Int], f: (String, Int) => String, z: String) =>
            unsafeRunSync(s.run(ZSink.foldLeft(z)(f))) == slurp(s).map(_.foldLeft(z)(f))
          }
        }
        test("fold") - propTest {
          forAll { (s: Stream[String, Int], f: (String, Int) => String, z: String) =>
            val ff = (acc: String, el: Int) => Step.more(f(acc, el))

            unsafeRunSync(s.run(ZSink.fold(z)(ff))) == slurp(s).map(_.foldLeft(z)(f))
          }

        }
        test("fold short circuits") - {
          val empty: Stream[Nothing, Int]     = ZStream.empty
          val single: Stream[Nothing, Int]    = ZStream.succeed(1)
          val double: Stream[Nothing, Int]    = ZStream(1, 2)
          val failed: Stream[String, Nothing] = ZStream.fail("Ouch")

          def run[E](stream: Stream[E, Int]) = {
            var effects: List[Int] = Nil
            val sink = ZSink.fold[Any, Int, Int](0) { (_, a) =>
              effects ::= a
              Step.done(30, Chunk.empty)
            }

            val exit: Exit[E, Int] = unsafeRunSync(stream.run(sink))

            (exit, effects)
          }

          assertTuple(run(empty), (Exit.succeed(0), Nil))
          assertTuple(run(single), (Exit.succeed(30), List(1)))
          assertTuple(run(double), (Exit.succeed(30), List(1)))
          assertTuple(run(failed), (Exit.fail("Ouch"), Nil))

        }
        test("foldM") - propTest {
          implicit val ioArb: Arbitrary[IO[String, String]] = Arbitrary(genSuccess[String, String])

          forAll { (s: Stream[String, Int], f: (String, Int) => IO[String, String], z: IO[String, String]) =>
            val ff         = (acc: String, el: Int) => f(acc, el).map(Step.more)
            val sinkResult = unsafeRunSync(z.flatMap(z => s.run(ZSink.foldM(z)(ff))))
            val foldResult = unsafeRunSync {
              s.foldLeft(List[Int]())((acc, el) => el :: acc)
                .use(IO.succeed)
                .map(_.reverse)
                .flatMap(_.foldLeft(z)((acc, el) => acc.flatMap(f(_, el))))
            }

            if (foldResult.succeeded) sinkResult == foldResult else true
          }

        }
        test("foldM short circuits") - {
          val empty: Stream[Nothing, Int]     = ZStream.empty
          val single: Stream[Nothing, Int]    = ZStream.succeed(1)
          val double: Stream[Nothing, Int]    = ZStream(1, 2)
          val failed: Stream[String, Nothing] = ZStream.fail("Ouch")

          def run[E](stream: Stream[E, Int]) = {
            var effects: List[Int] = Nil
            val sink = ZSink.foldM[Any, E, Int, Int, Int](0) { (_, a) =>
              effects ::= a
              IO.succeed(Step.done(30, Chunk.empty))
            }

            val exit = unsafeRunSync(stream.run(sink))

            (exit, effects)
          }

          assertTuple(run(empty), (Exit.succeed(0), Nil))
          assertTuple(run(single), (Exit.succeed(30), List(1)))
          assertTuple(run(double), (Exit.succeed(30), List(1)))
          assertTuple(run(failed), (Exit.fail("Ouch"), Nil))

        }
        test("collectAllWhile") - propTest {
          forAll { (s: Stream[String, String], f: String => Boolean) =>
            val sinkResult = unsafeRunSync(s.run(ZSink.collectAllWhile(f)))
            val listResult = slurp(s).map(_.takeWhile(f))

            if (listResult.succeeded) sinkResult == listResult else true
          }

        }
        test("foldWeighted") - {
          Stream[Long](1, 5, 2, 3)
            .transduce(Sink.foldWeighted(List[Long]())((_: Long) * 2, 12)((acc, el) => el :: acc).map(_.reverse))
            .runCollect
            .map(l => assert(l == List(List(1, 5), List(2, 3))))
        }
        test("foldWeightedM") - {
          Stream[Long](1, 5, 2, 3)
            .transduce(
              Sink
                .foldWeightedM(List[Long]())((a: Long) => UIO.succeed(a * 2), 12)((acc, el) => UIO.succeed(el :: acc))
                .map(_.reverse)
            )
            .runCollect
            .map(l => assert(l == List(List(1, 5), List(2, 3))))

        }
        test("foldUntil") - {
          unsafeRun {
            Stream[Long](1, 1, 1, 1, 1, 1)
              .transduce(Sink.foldUntil(0L, 3)(_ + (_: Long)))
              .runCollect
              .map(l => assert(l == List(3, 3)))
          }
        }
        test("foldUntilM") - {
          unsafeRun {
            Stream[Long](1, 1, 1, 1, 1, 1)
              .transduce(Sink.foldUntilM(0L, 3)((s, a: Long) => UIO.succeed(s + a)))
              .runCollect
              .map(l => assert(l == List(3, 3)))
          }

        }
        test("fromOutputStream") - {
          unsafeRun {
            import java.io.ByteArrayOutputStream

            val output = new ByteArrayOutputStream()
            val data   = "0123456789"
            val stream = Stream(Chunk.fromArray(data.take(5).getBytes), Chunk.fromArray(data.drop(5).getBytes))

            stream.run(ZSink.fromOutputStream(output)) map { bytesWritten =>
              assert(bytesWritten == 10)
              assert(new String(output.toByteArray, "UTF-8") == data)
            }
          }
        }
        test("throttleEnforce") - {
          def sinkTest(sink: ZSink[Clock, Nothing, Nothing, Int, Option[Int]]) =
            for {
              init1 <- sink.initial
              step1 <- sink.step(Step.state(init1), 1)
              res1  <- sink.extract(Step.state(step1))
              init2 <- sink.initial
              _     <- clock.sleep(23.milliseconds)
              step2 <- sink.step(Step.state(init2), 2)
              res2  <- sink.extract(Step.state(step2))
              init3 <- sink.initial
              step3 <- sink.step(Step.state(init3), 3)
              res3  <- sink.extract(Step.state(step3))
              init4 <- sink.initial
              step4 <- sink.step(Step.state(init4), 4)
              res4  <- sink.extract(Step.state(step4))
              _     <- clock.sleep(11.milliseconds)
              init5 <- sink.initial
              step5 <- sink.step(Step.state(init5), 5)
              res5  <- sink.extract(Step.state(step5))
            } yield assert(List(res1, res2, res3, res4, res5) == List(Some(1), Some(2), None, None, Some(5)))

          unsafeRun {
            for {
              clock <- Ref.make(TestClock.Zero).map(ref => new Clock { val clock = TestClock(ref) })
              test <- ZSink
                       .throttleEnforce[Int](1, 10.milliseconds)(_ => 1)
                       .use(sinkTest)
                       .provide(clock)
            } yield test
          }

        }
        test("throttleEnforce with burst") - {
          def sinkTest(sink: ZSink[Clock, Nothing, Nothing, Int, Option[Int]]) =
            for {
              init1 <- sink.initial
              step1 <- sink.step(Step.state(init1), 1)
              res1  <- sink.extract(Step.state(step1))
              init2 <- sink.initial
              _     <- clock.sleep(23.milliseconds)
              step2 <- sink.step(Step.state(init2), 2)
              res2  <- sink.extract(Step.state(step2))
              init3 <- sink.initial
              step3 <- sink.step(Step.state(init3), 3)
              res3  <- sink.extract(Step.state(step3))
              init4 <- sink.initial
              step4 <- sink.step(Step.state(init4), 4)
              res4  <- sink.extract(Step.state(step4))
              _     <- clock.sleep(11.milliseconds)
              init5 <- sink.initial
              step5 <- sink.step(Step.state(init5), 5)
              res5  <- sink.extract(Step.state(step5))
            } yield assert(List(res1, res2, res3, res4, res5) == List(Some(1), Some(2), Some(3), None, Some(5)))

          unsafeRun {
            for {
              clock <- Ref.make(TestClock.Zero).map(ref => new Clock { val clock = TestClock(ref) })
              test <- ZSink
                       .throttleEnforce[Int](1, 10.milliseconds, 1)(_ => 1)
                       .use(sinkTest)
                       .provide(clock)
            } yield test
          }

        }
        test("throttleShape") - {
          def sinkTest(sink: ZSink[Clock, Nothing, Nothing, Int, Int]) =
            for {
              init1   <- sink.initial
              step1   <- sink.step(Step.state(init1), 1)
              res1    <- sink.extract(Step.state(step1))
              init2   <- sink.initial
              step2   <- sink.step(Step.state(init2), 2)
              res2    <- sink.extract(Step.state(step2))
              init3   <- sink.initial
              _       <- clock.sleep(4.seconds)
              step3   <- sink.step(Step.state(init3), 3)
              res3    <- sink.extract(Step.state(step3))
              elapsed <- clock.currentTime(TimeUnit.SECONDS)
            } yield assert(elapsed == 8, List(res1, res2, res3) == List(1, 2, 3))

          unsafeRun {
            for {
              clock <- Ref.make(TestClock.Zero).map(ref => new Clock { val clock = TestClock(ref) })
              test <- ZSink
                       .throttleShape[Int](1, 1.second)(_.toLong)
                       .use(sinkTest)
                       .provide(clock)
            } yield test
          }

        }
        test("throttleShape infinite bandwidth") - {
          def sinkTest(sink: ZSink[Clock, Nothing, Nothing, Int, Int]) =
            for {
              init1   <- sink.initial
              step1   <- sink.step(Step.state(init1), 1)
              res1    <- sink.extract(Step.state(step1))
              init2   <- sink.initial
              step2   <- sink.step(Step.state(init2), 2)
              res2    <- sink.extract(Step.state(step2))
              elapsed <- clock.currentTime(TimeUnit.SECONDS)
            } yield assert(elapsed == 0, List(res1, res2) == List(1, 2))

          unsafeRun {
            for {
              clock <- Ref.make(TestClock.Zero).map(ref => new Clock { val clock = TestClock(ref) })
              test <- ZSink
                       .throttleShape[Int](1, 0.seconds)(_ => 100000L)
                       .use(sinkTest)
                       .provide(clock)
            } yield test
          }

        }
        test("throttleShape with burst") - {
          def sinkTest(sink: ZSink[Clock, Nothing, Nothing, Int, Int]) =
            for {
              init1   <- sink.initial
              step1   <- sink.step(Step.state(init1), 1)
              res1    <- sink.extract(Step.state(step1))
              init2   <- sink.initial
              step2   <- sink.step(Step.state(init2), 2)
              res2    <- sink.extract(Step.state(step2))
              init3   <- sink.initial
              _       <- clock.sleep(4.seconds)
              step3   <- sink.step(Step.state(init3), 3)
              res3    <- sink.extract(Step.state(step3))
              elapsed <- clock.currentTime(TimeUnit.SECONDS)
            } yield assert(elapsed == 6, List(res1, res2, res3) == List(1, 2, 3))

          unsafeRun {
            for {
              clock <- Ref.make(TestClock.Zero).map(ref => new Clock { val clock = TestClock(ref) })
              test <- ZSink
                       .throttleShape[Int](1, 1.second, 2)(_.toLong)
                       .use(sinkTest)
                       .provide(clock)
            } yield test
          }
        }
      }
    }
    test("Usecases") {
      test("Number array parsing with Sink.foldM") - {
        sealed trait ParserState
        object ParserState {
          case object Start               extends ParserState
          case class Element(acc: String) extends ParserState
          case object Done                extends ParserState
        }

        val numArrayParser =
          ZSink
            .foldM((ParserState.Start: ParserState, List.empty[Int])) { (s, a: Char) =>
              s match {
                case (ParserState.Start, acc) =>
                  a match {
                    case a if a.isWhitespace => IO.succeed(ZSink.Step.more((ParserState.Start, acc)))
                    case '['                 => IO.succeed(ZSink.Step.more((ParserState.Element(""), acc)))
                    case _                   => IO.fail("Expected '['")
                  }

                case (ParserState.Element(el), acc) =>
                  a match {
                    case a if a.isDigit => IO.succeed(ZSink.Step.more((ParserState.Element(el + a), acc)))
                    case ','            => IO.succeed(ZSink.Step.more((ParserState.Element(""), acc :+ el.toInt)))
                    case ']'            => IO.succeed(ZSink.Step.done((ParserState.Done, acc :+ el.toInt), Chunk.empty))
                    case _              => IO.fail("Expected a digit or ,")
                  }

                case (ParserState.Done, acc) =>
                  IO.succeed(ZSink.Step.done((ParserState.Done, acc), Chunk.empty))
              }
            }
            .map(_._2)
            .chunked

        val src1         = ZStreamChunk.succeedLazy(Chunk.fromArray(Array('[', '1', '2')))
        val src2         = ZStreamChunk.succeedLazy(Chunk.fromArray(Array('3', ',', '4', ']')))
        val partialParse = unsafeRunSync(src1.run(numArrayParser))
        val fullParse    = unsafeRunSync((src1 ++ src2).run(numArrayParser))

        assert(partialParse == Exit.Success(List()), fullParse == (Exit.Success(List(123, 4))))
      }
      test("Number array parsing with combinators") - {
        val comma: ZSink[Any, Nothing, Char, Char, List[Char]] = ZSink.collectAllWhile[Char](_ == ',')
        val brace: ZSink[Any, String, Char, Char, Char] =
          ZSink.read1[String, Char](a => s"Expected closing brace; instead: $a")((_: Char) == ']')
        val number: ZSink[Any, String, Char, Char, Int] =
          ZSink.collectAllWhile[Char](_.isDigit).map(_.mkString.toInt)
        val numbers = (number <*> (comma *> number).collectAllWhile[Char, Char](_ != ']'))
          .map(tp => tp._1 :: tp._2)

        val elements = numbers <* brace

        lazy val start: ZSink[Any, String, Char, Char, List[Int]] =
          ZSink.pull1(IO.fail("Input was empty")) {
            case a if a.isWhitespace => start
            case '['                 => elements
            case _                   => ZSink.fail("Expected '['")
          }

        val src1         = ZStreamChunk.succeedLazy(Chunk.fromArray(Array('[', '1', '2')))
        val src2         = ZStreamChunk.succeedLazy(Chunk.fromArray(Array('3', ',', '4', ']')))
        val partialParse = unsafeRunSync(src1.run(start.chunked))
        val fullParse    = unsafeRunSync((src1 ++ src2).run(start.chunked))

        assert(
          partialParse == Exit.fail("Expected closing brace; instead: None"),
          fullParse == Exit.Success(List(123, 4))
        )

      }
    }
  }
}
