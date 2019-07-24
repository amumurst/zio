package zio.stream

import org.scalacheck.Prop.forAll
import utest._
import zio.QueueSpec.waitForSize

import scala.{ Stream => _ }
import zio._
import zio.duration._
import ArbitraryChunk._
import ArbitraryStream._
import Exit.{ Cause => _, _ }
import org.scalacheck.Arbitrary
import zio.Cause

object ZStreamSpec extends TestRuntime with StreamTestUtils with GenIO with UtestScalacheckExtension {
  override def tests: Tests = Tests {
    test("Stream.aggregate") {
      test("aggregate") - {
        unsafeRun {
          Stream(1, 1, 1, 1)
            .aggregate(ZSink.foldUntil(List[Int](), 3)((acc, el: Int) => el :: acc).map(_.reverse))
            .runCollect
            .map { result =>
              assert(result.flatten == List(1, 1, 1, 1), result.forall(_.length <= 3))
            }
        }
      }
      test("error propagation") - {
        unsafeRun {
          val e    = new RuntimeException("Boom")
          val sink = ZSink.die(e)
          Stream(1, 1, 1, 1)
            .aggregate(sink)
            .runCollect
            .run
            .map(exit => assert(exit == Exit.Failure(Cause.Die(e))))
        }

      }
      test("error propagation") - {
        unsafeRun {
          val e = new RuntimeException("Boom")
          val sink = Sink.foldM[Nothing, Int, Int, List[Int]](List[Int]()) { (_, _) =>
            ZIO.die(e)
          }

          Stream(1, 1)
            .aggregate(sink)
            .runCollect
            .run
            .map(exit => exit == Exit.Failure(Cause.Die(e)))
        }

      }
      test("interruption propagation") {
        test {
          unsafeRun {
            for {
              latch     <- Promise.make[Nothing, Unit]
              cancelled <- Ref.make(false)
              sink = Sink.foldM[Nothing, Int, Int, List[Int]](List[Int]()) { (acc, el) =>
                if (el == 1) UIO.succeed(ZSink.Step.more(el :: acc))
                else
                  (latch.succeed(()) *> UIO.never)
                    .onInterrupt(cancelled.set(true))
              }
              fiber  <- Stream(1, 1, 2).aggregate(sink).runCollect.untraced.fork
              _      <- latch.await
              _      <- fiber.interrupt
              result <- cancelled.get
            } yield assert(result)
          }
        }
        test {
          unsafeRun {
            for {
              latch     <- Promise.make[Nothing, Unit]
              cancelled <- Ref.make(false)
              sink = Sink.fromEffect {
                (latch.succeed(()) *> UIO.never)
                  .onInterrupt(cancelled.set(true))
              }
              fiber  <- Stream(1, 1, 2).aggregate(sink).runCollect.untraced.fork
              _      <- latch.await
              _      <- fiber.interrupt
              result <- cancelled.get
            } yield assert(result)
          }
        }

      }
    }
    test("Stream.aggregateWithin") {
      test("aggregateWithin") - {
        unsafeRun {
          for {
            result <- Stream(1, 1, 1, 1, 2)
                       .aggregateWithin(
                         Sink.fold(List[Int]())(
                           (acc, el: Int) =>
                             if (el == 1) ZSink.Step.more(el :: acc)
                             else if (el == 2 && acc.isEmpty) ZSink.Step.done(el :: acc, Chunk.empty)
                             else ZSink.Step.done(acc, Chunk.single(el))
                         ),
                         ZSchedule.spaced(30.minutes)
                       )
                       .runCollect
          } yield assert(result == List(Right(List(1, 1, 1, 1)), Right(List(2))))
        }
      }
      test("error propagation") {
        test {
          unsafeRun {
            val e    = new RuntimeException("Boom")
            val sink = ZSink.die(e)
            Stream(1, 1, 1, 1)
              .aggregateWithin(sink, Schedule.spaced(30.minutes))
              .runCollect
              .run
              .map(exit => assert(exit == Exit.Failure(Cause.Die(e))))
          }
        }
        test {
          unsafeRun {
            val e = new RuntimeException("Boom")
            val sink = Sink.foldM[Nothing, Int, Int, List[Int]](List[Int]()) { (_, _) =>
              ZIO.die(e)
            }

            Stream(1, 1)
              .aggregateWithin(sink, Schedule.spaced(30.minutes))
              .runCollect
              .run
              .map(exit => assert(exit == Exit.Failure(Cause.Die(e))))
          }
        }

      }
      test("interruption propagation") {
        test {
          unsafeRun {
            for {
              latch     <- Promise.make[Nothing, Unit]
              cancelled <- Ref.make(false)
              sink = Sink.foldM[Nothing, Int, Int, List[Int]](List[Int]()) { (acc, el) =>
                if (el == 1) UIO.succeed(ZSink.Step.more(el :: acc))
                else
                  (latch.succeed(()) *> UIO.never)
                    .onInterrupt(cancelled.set(true))
              }
              fiber  <- Stream(1, 1, 2).aggregateWithin(sink, Schedule.spaced(30.minutes)).runCollect.untraced.fork
              _      <- latch.await
              _      <- fiber.interrupt
              result <- cancelled.get
            } yield assert(result)
          }
        }
        test {
          unsafeRun {
            for {
              latch     <- Promise.make[Nothing, Unit]
              cancelled <- Ref.make(false)
              sink = Sink.fromEffect {
                (latch.succeed(()) *> UIO.never)
                  .onInterrupt(cancelled.set(true))
              }
              fiber  <- Stream(1, 1, 2).aggregateWithin(sink, Schedule.spaced(30.minutes)).runCollect.untraced.fork
              _      <- latch.await
              _      <- fiber.interrupt
              result <- cancelled.get
            } yield assert(result)
          }
        }
      }
    }
    test("Stream.bracket") {
      test("bracket") - {
        unsafeRun(
          for {
            done           <- Ref.make(false)
            iteratorStream = Stream.bracket(UIO(0 to 2))(_ => done.set(true)).flatMap(Stream.fromIterable)
            result         <- iteratorStream.run(Sink.collectAll[Int])
            released       <- done.get
          } yield assert(result == List(0, 1, 2), released)
        )

      }
      test("bracket short circuits") - {
        unsafeRun(
          for {
            done <- Ref.make(false)
            iteratorStream = Stream
              .bracket(UIO(0 to 3))(_ => done.set(true))
              .flatMap(Stream.fromIterable)
              .take(2)
            result   <- iteratorStream.run(Sink.collectAll[Int])
            released <- done.get
          } yield assert(result == List(0, 1), released)
        )

      }
      test("no acquisition when short circuiting") - {
        unsafeRun(
          for {
            acquired       <- Ref.make(false)
            iteratorStream = (Stream(1) ++ Stream.bracket(acquired.set(true))(_ => UIO.unit)).take(0)
            _              <- iteratorStream.run(Sink.drain)
            result         <- acquired.get
          } yield assert(!result)
        )
      }
      test("releases when there are defects") - {
        unsafeRun {
          for {
            ref <- Ref.make(false)
            _ <- Stream
                  .bracket(ZIO.unit)(_ => ref.set(true))
                  .flatMap(_ => Stream.fromEffect(ZIO.dieMessage("boom")))
                  .run(Sink.drain)
                  .run
            released <- ref.get
          } yield assert(released)
        }
      }
    }
    test("Stream.buffer") {
      test("buffer the Stream") - propTest {
        forAll { list: List[Int] =>
          unsafeRunSync(
            Stream
              .fromIterable(list)
              .buffer(2)
              .run(Sink.collectAll[Int])
          ) == Success(list)
        }
      }
      test("buffer the Stream with Error") - {
        val e = new RuntimeException("boom")
        val result = unsafeRunSync(
          (Stream.range(0, 10) ++ Stream.fail(e))
            .buffer(2)
            .run(Sink.collectAll[Int])
        )
        assert(result == Failure(Cause.Fail(e)))
      }
      test("fast producer progress independently") - {
        unsafeRun(
          for {
            promise <- Promise.make[Nothing, Unit]
            ref     <- Ref.make(List[Int]())
            _ <- Stream
                  .range(1, 5)
                  .mapM(i => ref.update(i :: _) <* promise.succeed(()).when(i == 4))
                  .buffer(2)
                  .mapM(_ => IO.never)
                  .runDrain
                  .fork
            _    <- promise.await
            list <- ref.get
            // 1 element stuck in the second mapM, 2 elements buffered,
            // 1 element waiting to be enqueued to the buffer
          } yield assert(list.reverse == (1 to 4).toList)
        )
      }
    }
    test("Stream.collect") - {
      val s = Stream(Left(1), Right(2), Left(3)).collect {
        case Right(n) => n
      }

      assert(slurp(s) == Success(List(2)), slurp(s) == Success(List(2)))
    }
    test("Stream.collectWhile") {
      test("collectWhile") - {
        val s = Stream(Left(1), Right(2), Left(3)).collect {
          case Right(n) => n
        }

        assert(slurp(s) == Success(List(2)), slurp(s) == Success(List(2)))
      }
      test("collectWhile short circuits") - {
        unsafeRun {
          (Stream(Option(1)) ++ Stream.fail("Ouch")).collectWhile {
            case None => 1
          }.runDrain.either
            .map(e => assert(e.isRight))
        }
      }
    }
    test("Stream.concat") {
      test("concat") - propTest {
        forAll { (s1: Stream[String, Byte], s2: Stream[String, Byte]) =>
          val listConcat = (slurp(s1) zip slurp(s2)).map {
            case (left, right) => left ++ right
          }
          val streamConcat = slurp(s1 ++ s2)
          if (streamConcat.succeeded && listConcat.succeeded) streamConcat == listConcat else true
        }

      }
      test("finalizer order") - {
        unsafeRun {
          for {
            log       <- Ref.make[List[String]](Nil)
            _         <- (Stream.finalizer(log.update("Second" :: _)) ++ Stream.finalizer(log.update("First" :: _))).runDrain
            execution <- log.get
          } yield assert(execution == List("Second", "First"))
        }
      }
    }
    test("Stream.drain") {
      unsafeRun(
        for {
          ref <- Ref.make(List[Int]())
          _   <- Stream.range(0, 10).mapM(i => ref.update(i :: _)).drain.run(Sink.drain)
          l   <- ref.get
        } yield assert(l.reverse == (0 to 10).toList)
      )
    }
    test("Stream.dropWhile") {
      test("dropWhile") - propTest {
        forAll { (s: Stream[String, Byte], p: Byte => Boolean) =>
          slurp(s.dropWhile(p)) == slurp(s).map(_.dropWhile(p))
        }
      }
      test("short circuits") - {
        unsafeRun {
          (Stream(1) ++ Stream.fail("Ouch"))
            .take(1)
            .dropWhile(_ => true)
            .runDrain
            .either
            .map(e => e.isRight)
        }
      }
    }
    test("Stream.effectAsync") - propTest {
      forAll { list: List[Int] =>
        val s = Stream.effectAsync[Throwable, Int] { k =>
          list.foreach(a => k(Task.succeed(a)))
        }

        slurp(s.take(list.size)) == Success(list)
      }
    }
    test("Stream.effectAsyncMaybe") {
      test("effectAsyncMaybe Some") - propTest {
        forAll { list: List[Int] =>
          val s = Stream.effectAsyncMaybe[Throwable, Int] { _ =>
            Some(Stream.fromIterable(list))
          }

          slurp(s.take(list.size)) == Success(list)
        }
      }
      test("effectAsyncMaybe None") - propTest {
        forAll { list: List[Int] =>
          val s = Stream.effectAsyncMaybe[Throwable, Int] { k =>
            list.foreach(a => k(Task.succeed(a)))
            None
          }

          slurp(s.take(list.size)) == Success(list)
        }
      }
    }
    test("Stream.effectAsyncM") {
      val list = List(1, 2, 3)
      unsafeRun {
        for {
          latch <- Promise.make[Nothing, Unit]
          fiber <- ZStream
                    .effectAsyncM[Any, Throwable, Int] { k =>
                      latch.succeed(()) *>
                        Task.succeedLazy {
                          list.foreach(a => k(Task.succeed(a)))
                        }
                    }
                    .take(list.size)
                    .run(Sink.collectAll[Int])
                    .fork
          _ <- latch.await
          s <- fiber.join
        } yield assert(s == list)
      }
    }
    test("Stream.effectAsyncInterrupt") {
      test("effectAsyncInterrupt Left") - {
        unsafeRun {
          for {
            cancelled <- Ref.make(false)
            latch     <- Promise.make[Nothing, Unit]
            fiber <- Stream
                      .effectAsyncInterrupt[Nothing, Unit] { offer =>
                        offer(ZIO.succeed(())); Left(cancelled.set(true))
                      }
                      .tap(_ => latch.succeed(()))
                      .run(Sink.collectAll[Unit])
                      .fork
            _      <- latch.await
            _      <- fiber.interrupt
            result <- cancelled.get
          } yield assert(result)
        }
      }
      test("effectAsyncInterrupt Right") - propTest {
        forAll { list: List[Int] =>
          val s = Stream.effectAsyncInterrupt[Throwable, Int] { _ =>
            Right(Stream.fromIterable(list))
          }

          slurp(s.take(list.size)) == Success(list)
        }
      }
    }
    test("Stream.ensuring") {
      unsafeRun {
        for {
          log <- Ref.make[List[String]](Nil)
          _ <- (for {
                _ <- Stream.bracket(log.update("Acquire" :: _))(_ => log.update("Release" :: _))
                _ <- Stream.fromEffect(log.update("Use" :: _))
              } yield ()).ensuring(log.update("Ensuring" :: _)).runDrain
          execution <- log.get
        } yield assert(execution == List("Ensuring", "Release", "Use", "Acquire"))
      }
    }
    test("Stream.finalizer") {
      unsafeRun {
        for {
          log <- Ref.make[List[String]](Nil)
          _ <- (for {
                _ <- Stream.bracket(log.update("Acquire" :: _))(_ => log.update("Release" :: _))
                _ <- Stream.finalizer(log.update("Use" :: _))
              } yield ()).ensuring(log.update("Ensuring" :: _)).runDrain
          execution <- log.get
        } yield assert(execution == List("Ensuring", "Release", "Use", "Acquire"))
      }
    }
    test("Stream.filter") {
      test("filter") - propTest {
        forAll { (s: Stream[String, Byte], p: Byte => Boolean) =>
          slurp(s.filter(p)) == slurp(s).map(_.filter(p))
        }
      }
      test("short circuits #1") - {
        unsafeRun {
          (Stream(1) ++ Stream.fail("Ouch"))
            .filter(_ => true)
            .take(1)
            .runDrain
            .either
            .map(e => assert(e.isRight))
        }
      }
      test("short circuits #2") - {
        unsafeRun {
          (Stream(1) ++ Stream.fail("Ouch"))
            .take(1)
            .filter(_ => true)
            .runDrain
            .either
            .map(e => assert(e.isRight))
        }
      }
    }
    test("Stream.filterM") {
      test("filterM") - propTest {
        forAll { (s: Stream[String, Byte], p: Byte => Boolean) =>
          slurp(s.filterM(s => IO.succeed(p(s)))) == slurp(s).map(_.filter(p))
        }

      }
      test("short circuits #1") - {
        unsafeRun {
          (Stream(1) ++ Stream.fail("Ouch"))
            .take(1)
            .filterM(_ => UIO.succeed(true))
            .runDrain
            .either
            .map(e => assert(e.isRight))
        }
      }
      test("short circuits #2") - {
        unsafeRun {
          (Stream(1) ++ Stream.fail("Ouch"))
            .filterM(_ => UIO.succeed(true))
            .take(1)
            .runDrain
            .either
            .map(e => assert(e.isRight))
        }
      }
    }
    test("Stream.flatMap") {
      test("deep flatMap stack safety") - {
        def fib(n: Int): Stream[Nothing, Int] =
          if (n <= 1) Stream.succeedLazy(n)
          else
            fib(n - 1).flatMap { a =>
              fib(n - 2).flatMap { b =>
                Stream.succeedLazy(a + b)
              }
            }

        val stream   = fib(20)
        val expected = 6765

        assert(slurp(stream).toEither == Right(List(expected)))
      }
      test("left identity") - propTest {
        forAll((x: Int, f: Int => Stream[String, Int]) => slurp(Stream(x).flatMap(f)) == slurp(f(x)))
      }
      test("right identity") - propTest {
        forAll((m: Stream[String, Int]) => slurp(m.flatMap(i => Stream(i))) == slurp(m))
      }
      test("associativity") - propTest {
        forAll { (m: Stream[String, Int], f: Int => Stream[String, Int], g: Int => Stream[String, Int]) =>
          val leftStream  = m.flatMap(f).flatMap(g)
          val rightStream = m.flatMap(x => f(x).flatMap(g))
          slurp(leftStream) == slurp(rightStream)
        }
      }
    }
    test("Stream.flatMapPar/flattenPar/mergeAll") {
      test("consistent with flatMap") - propTest {
        forAll { (n: Long, m: List[Int]) =>
          val flatMap    = Stream.fromIterable(m).flatMap(i => Stream(i, i))
          val flatMapPar = Stream.fromIterable(m).flatMapPar(n)(i => Stream(i, i))

          if (n > 0) slurp(flatMap).map(_.toSet) == slurp(flatMapPar).map(_.toSet) else true
        }
      }
      test("short circuiting") - {
        unsafeRun {
          Stream
            .mergeAll(2)(
              Stream.never,
              Stream(1)
            )
            .take(1)
            .run(Sink.collectAll[Int])
            .map(l => assert(l == List(1)))
        }
      }
      test("interruption propagation") - {
        unsafeRun {
          for {
            substreamCancelled <- Ref.make[Boolean](false)
            latch              <- Promise.make[Nothing, Unit]
            fiber <- Stream(())
                      .flatMapPar(1)(
                        _ =>
                          Stream.fromEffect((latch.succeed(()) *> ZIO.never).onInterrupt(substreamCancelled.set(true)))
                      )
                      .run(Sink.collectAll[Unit])
                      .fork
            _         <- latch.await
            _         <- fiber.interrupt
            cancelled <- substreamCancelled.get
          } yield assert(cancelled)
        }
      }
      test("inner errors interrupt all fibers") - {
        unsafeRun {
          for {
            substreamCancelled <- Ref.make[Boolean](false)
            latch              <- Promise.make[Nothing, Unit]
            result <- Stream(
                       Stream.fromEffect((latch.succeed(()) *> ZIO.never).onInterrupt(substreamCancelled.set(true))),
                       Stream.fromEffect(latch.await *> ZIO.fail("Ouch"))
                     ).flatMapPar(2)(identity)
                       .run(Sink.drain)
                       .either
            cancelled <- substreamCancelled.get
          } yield assert(cancelled, result == Left("Ouch"))
        }
      }
      test("outer errors interrupt all fibers") - {
        unsafeRun {
          for {
            substreamCancelled <- Ref.make[Boolean](false)
            latch              <- Promise.make[Nothing, Unit]
            result <- (Stream(()) ++ Stream.fromEffect(latch.await *> ZIO.fail("Ouch")))
                       .flatMapPar(2) { _ =>
                         Stream.fromEffect((latch.succeed(()) *> ZIO.never).onInterrupt(substreamCancelled.set(true)))
                       }
                       .run(Sink.drain)
                       .either
            cancelled <- substreamCancelled.get
          } yield assert(cancelled, result == Left("Ouch"))
        }
      }
      test("inner defects interrupt all fibers") - {
        unsafeRun {
          val ex = new RuntimeException("Ouch")

          for {
            substreamCancelled <- Ref.make[Boolean](false)
            latch              <- Promise.make[Nothing, Unit]
            result <- Stream(
                       Stream.fromEffect((latch.succeed(()) *> ZIO.never).onInterrupt(substreamCancelled.set(true))),
                       Stream.fromEffect(latch.await *> ZIO.die(ex))
                     ).flatMapPar(2)(identity)
                       .run(Sink.drain)
                       .run
            cancelled <- substreamCancelled.get
          } yield assert(cancelled, result == Exit.die(ex))
        }

      }
      test("outer defects interrupt all fibers") - {
        unsafeRun {
          val ex = new RuntimeException("Ouch")

          for {
            substreamCancelled <- Ref.make[Boolean](false)
            latch              <- Promise.make[Nothing, Unit]
            result <- Stream(
                       Stream.fromEffect((latch.succeed(()) *> ZIO.never).onInterrupt(substreamCancelled.set(true))),
                       Stream.fromEffect(latch.await *> ZIO.die(ex))
                     ).flatMapPar(2)(identity)
                       .run(Sink.drain)
                       .run
            cancelled <- substreamCancelled.get
          } yield assert(cancelled, result == Exit.die(ex))
        }
      }
      test("finalizer ordering") - {
        unsafeRun {
          for {
            execution <- Ref.make[List[String]](Nil)
            inner = Stream
              .bracket(execution.update("InnerAcquire" :: _))(_ => execution.update("InnerRelease" :: _))
            _ <- Stream
                  .bracket(execution.update("OuterAcquire" :: _).const(inner))(
                    _ => execution.update("OuterRelease" :: _)
                  )
                  .flatMapPar(2)(identity)
                  .runDrain
            results <- execution.get
          } yield assert(results == List("OuterRelease", "InnerRelease", "InnerAcquire", "OuterAcquire"))
        }
      }
    }
    test("Stream.foreach/foreachWhile") {
      test("foreach") - {
        {
          var sum = 0
          val s   = Stream(1, 1, 1, 1, 1)

          unsafeRun(s.foreach[Any, Nothing](a => IO.effectTotal(sum += a)))
          assert(sum == 5)
        }
      }
      test("foreachWhile") - {
        var sum = 0
        val s   = Stream(1, 1, 1, 1, 1, 1)

        unsafeRun(
          s.foreachWhile[Any, Nothing](
            a =>
              IO.effectTotal(
                if (sum >= 3) false
                else {
                  sum += a;
                  true
                }
              )
          )
        )
        assert(sum == 3)
      }
      test("foreachWhile short circuits") - {
        unsafeRun {
          for {
            flag    <- Ref.make(true)
            _       <- (Stream(true, true, false) ++ Stream.fromEffect(flag.set(false)).drain).foreachWhile(ZIO.succeed)
            skipped <- flag.get
          } yield assert(skipped)
        }
      }
    }
    test("Stream.forever") - {
      var sum = 0
      val s = Stream(1).forever.foreachWhile[Any, Nothing](
        a =>
          IO.effectTotal {
            sum += a;
            if (sum >= 9) false else true
          }
      )

      unsafeRun(s)
      assert(sum == 9)
    }
    test("Stream.fromChunk") - propTest {
      forAll { c: Chunk[Int] =>
        val s = Stream.fromChunk(c)
        (slurp(s) == Success(c.toSeq.toList)) && (slurp(s) == Success(c.toSeq.toList))
      }
    }
    test("Stream.fromInputStream") - {
      unsafeRun {
        import java.io.ByteArrayInputStream
        val chunkSize = ZStreamChunk.DefaultChunkSize
        val data      = Array.tabulate[Byte](chunkSize * 5 / 2)(_.toByte)
        val is        = new ByteArrayInputStream(data)
        ZStream.fromInputStream(is, chunkSize).run(Sink.collectAll[Chunk[Byte]]) map { chunks =>
          assert(chunks.flatMap(_.toArray[Byte]).sameElements(data))
        }
      }
    }
    test("Stream.fromIterable") - propTest {
      forAll { l: List[Int] =>
        val s = Stream.fromIterable(l)
        slurp(s) == Success(l) && (slurp(s) == Success(l))
      }
    }
    test("Stream.fromQueue") - propTest {
      forAll { c: Chunk[Int] =>
        val result = unsafeRunSync {
          for {
            queue <- Queue.unbounded[Int]
            _     <- queue.offerAll(c.toSeq)
            fiber <- Stream
                      .fromQueue(queue)
                      .fold[Any, Nothing, Int, List[Int]]
                      .flatMap { fold =>
                        fold(List[Int](), _ => true, (acc, el) => IO.succeed(el :: acc))
                      }
                      .use(ZIO.succeed)
                      .map(_.reverse)
                      .fork
            _     <- waitForSize(queue, -1)
            _     <- queue.shutdown
            items <- fiber.join
          } yield items

        }
        result == Success(c.toSeq.toList)
      }
    }
    test("Stream.map") - propTest {
      forAll { (s: Stream[String, Byte], f: Byte => Int) =>
        slurp(s.map(f)) == slurp(s).map(_.map(f))
      }
    }
    test("Stream.mapAccum") - {
      val stream = Stream(1, 1, 1).mapAccum(0)((acc, el) => (acc + el, acc + el))
      assert(slurp(stream) == Success(List(1, 2, 3)))
    }
    test("Stream.mapAccumM") - {
      val stream = Stream(1, 1, 1).mapAccumM[Any, Nothing, Int, Int](0)((acc, el) => IO.succeed((acc + el, acc + el)))
      assert(slurp(stream) == Success(List(1, 2, 3)), slurp(stream) == Success(List(1, 2, 3)))
    }
    test("Stream.mapConcat") - propTest {
      forAll { (s: Stream[String, Byte], f: Byte => Chunk[Int]) =>
        slurp(s.mapConcat(f)) == slurp(s).map(_.flatMap(v => f(v).toSeq))
      }
    }
    test("Stream.mapM") - propTest {
      implicit val arb: Arbitrary[IO[String, Byte]] = Arbitrary(genIO[String, Byte])

      forAll { (data: List[Byte], f: Byte => IO[String, Byte]) =>
        unsafeRun {
          val s = Stream.fromIterable(data)

          for {
            l <- s.mapM(f).runCollect.either
            r <- IO.foreach(data)(f).either
          } yield l == r
        }
      }
    }
    test("Stream.repeatEffect") - {
      unsafeRun(
        Stream
          .repeatEffect(IO.succeed(1))
          .take(2)
          .run(Sink.collectAll[Int])
          .map(l => assert(l == List(1, 1)))
      )
    }
    test("Stream.repeatEffectWith") - {
      unsafeRun(
        for {
          ref <- Ref.make[List[Int]](Nil)
          _ <- Stream
                .repeatEffectWith(ref.update(1 :: _), Schedule.spaced(10.millis))
                .take(2)
                .run(Sink.drain)
          result <- ref.get
        } yield assert(result == List(1, 1))
      )
    }
    test("Stream.mapMPar") {
      test("foreachParN equivalence") - propTest {
        implicit val arb: Arbitrary[IO[Unit, Byte]] = Arbitrary(genIO[Unit, Byte])

        forAll { (data: List[Byte], f: Byte => IO[Unit, Byte]) =>
          unsafeRun {
            val s = Stream.fromIterable(data)

            for {
              l <- s.mapMPar(8)(f).runCollect.either
              r <- IO.foreachParN(8)(data)(f).either
            } yield l == r
          }
        }

      }
      test("interruption propagation") - {
        unsafeRun {
          for {
            interrupted <- Ref.make(false)
            latch       <- Promise.make[Nothing, Unit]
            fib <- Stream(())
                    .mapMPar(1) { _ =>
                      (latch.succeed(()) *> ZIO.never).onInterrupt(interrupted.set(true))
                    }
                    .runDrain
                    .fork
            _      <- latch.await
            _      <- fib.interrupt
            result <- interrupted.get
          } yield assert(result)
        }
      }
    }
    test("Stream merging") {
      test("merge") - propTest {
        forAll { (s1: Stream[String, Int], s2: Stream[String, Int]) =>
          val mergedStream = slurp(s1 merge s2).map(_.toSet)
          val mergedLists  = (slurp(s1) zip slurp(s2)).map { case (left, right) => left ++ right }.map(_.toSet)
          (!mergedStream.succeeded && !mergedLists.succeeded) || (mergedStream == mergedLists)
        }

      }
      test("mergeEither") - {
        val s1 = Stream(1, 2)
        val s2 = Stream(1, 2)

        val merge = s1.mergeEither(s2)
        val list: List[Either[Int, Int]] = slurp(merge).toEither.fold(
          _ => List.empty,
          identity
        )

        val elements = List(Left(1), Left(2), Right(1), Right(2))

        assert(list.forall(elements.contains), elements.forall(list.contains))
      }
      test("mergeWith") - {
        val s1 = Stream(1, 2)
        val s2 = Stream(1, 2)

        val merge = s1.mergeWith(s2)(_.toString, _.toString)
        val list: List[String] = slurp(merge).toEither.fold(
          _ => List.empty,
          identity
        )

        val elements = List("1", "2", "1", "2")

        assert(list.forall(elements.contains), elements.forall(list.contains))
      }
      test("mergeWith short circuit") - {
        val s1 = Stream(1, 2)
        val s2 = Stream(1, 2)

        val merge = s1.mergeWith(s2)(_.toString, _.toString)
        val list: List[String] = slurp0(merge)(_ => false).toEither.fold(
          _ => List("9"),
          identity
        )

        assert(list.isEmpty)
      }
      test("mergeWith prioritizes failure") - {
        val s1 = Stream.never
        val s2 = Stream.fail("Ouch")

        s1.mergeWith(s2)(_ => (), _ => ())
          .runCollect
          .either
          .map(e => assert(e == Left("Ouch")))
      }
    }
    test("Stream.peel") - {
      val s      = Stream('1', '2', ',', '3', '4')
      val parser = ZSink.collectAllWhile[Char](_.isDigit).map(_.mkString.toInt) <* ZSink.collectAllWhile[Char](_ == ',')
      val peeled = s.peel(parser).use[Any, Int, (Int, Exit[Nothing, List[Char]])] {
        case (n, rest) =>
          IO.succeed((n, slurp(rest)))
      }

      assertTuple(unsafeRun(peeled), (12, Success(List('3', '4'))))
    }
    test("Stream.range") - {
      val s = Stream.range(0, 9)
      assert(slurp(s) == Success((0 to 9).toList), slurp(s) == Success((0 to 9).toList))
    }
    test("Stream.repeat") {
      test("repeat") - {
        unsafeRun(
          Stream(1)
            .repeat(Schedule.recurs(4))
            .run(Sink.collectAll[Int])
            .map(l => assert(l == List(1, 1, 1, 1, 1)))
        )
      }
      test("short circuits") - {
        unsafeRun(
          for {
            ref <- Ref.make[List[Int]](Nil)
            _ <- Stream
                  .fromEffect(ref.update(1 :: _))
                  .repeat(Schedule.spaced(10.millis))
                  .take(2)
                  .run(Sink.drain)
            result <- ref.get
          } yield assert(result == List(1, 1))
        )
      }
    }
    test("Stream.spaced") {
      test("spaced") - {
        unsafeRun(
          Stream("A", "B", "C")
            .spaced(Schedule.recurs(0) *> Schedule.fromFunction((_) => "!"))
            .run(Sink.collectAll[String])
            .map(l => assert(l == List("A", "!", "B", "!", "C", "!")))
        )
      }
      test("spacedEither") - {
        unsafeRun(
          Stream("A", "B", "C")
            .spacedEither(Schedule.recurs(0) *> Schedule.fromFunction((_) => 123))
            .run(Sink.collectAll[Either[Int, String]])
            .map(l => assert(l == List(Right("A"), Left(123), Right("B"), Left(123), Right("C"), Left(123))))
        )
      }
      test("repeated and spaced") - {
        unsafeRun(
          Stream("A", "B", "C")
            .spaced(Schedule.recurs(1) *> Schedule.fromFunction((_) => "!"))
            .run(Sink.collectAll[String])
            .map(l => assert(l == List("A", "A", "!", "B", "B", "!", "C", "C", "!")))
        )
      }
      test("short circuits in schedule") - {
        unsafeRun(
          Stream("A", "B", "C")
            .spaced(Schedule.recurs(1) *> Schedule.fromFunction((_) => "!"))
            .take(3)
            .run(Sink.collectAll[String])
            .map(l => assert(l == List("A", "A", "!")))
        )
      }
      test("short circuits after schedule") - {
        unsafeRun(
          Stream("A", "B", "C")
            .spaced(Schedule.recurs(1) *> Schedule.fromFunction((_) => "!"))
            .take(4)
            .run(Sink.collectAll[String])
            .map(l => assert(l == List("A", "A", "!", "B")))
        )
      }
    }
    test("Stream.take") {
      test("take") - propTest {
        forAll { (s: Stream[String, Byte], n: Int) =>
          val takeStreamesult = slurp(s.take(n))
          val takeListResult  = slurp(s).map(_.take(n))
          if (takeListResult.succeeded) takeStreamesult == takeListResult else true
        }
      }
      test("take short circuits") - {
        unsafeRun(
          for {
            ran    <- Ref.make(false)
            stream = (Stream(1) ++ Stream.fromEffect(ran.set(true)).drain).take(0)
            _      <- stream.run(Sink.drain)
            result <- ran.get
          } yield assert(!result)
        )
      }
      test("take(0) short circuits") - {
        unsafeRun(
          for {
            units <- Stream.never.take(0).run(Sink.collectAll[Unit])
          } yield assert(units.isEmpty)
        )
      }
      test("take(1) short circuits") - {
        unsafeRun(
          for {
            ints <- (Stream(1) ++ Stream.never).take(1).run(Sink.collectAll[Int])
          } yield assert(ints == List(1))
        )

      }
      test("takeWhile") - propTest {
        forAll { (s: Stream[String, Byte], p: Byte => Boolean) =>
          val streamTakeWhile = slurp(s.takeWhile(p))
          val listTakeWhile   = slurp(s).map(_.takeWhile(p))

          if (listTakeWhile.succeeded) streamTakeWhile == listTakeWhile else true
        }
      }
      test("takeWhile short circuits") - {
        unsafeRun(
          (Stream(1) ++ Stream.fail("Ouch"))
            .takeWhile(_ => false)
            .runDrain
            .either
            .map(e => assert(e.isRight))
        )
      }
    }
    test("Stream.tap") - {
      var sum     = 0
      val s       = Stream(1, 1).tap[Any, Nothing](a => IO.effectTotal(sum += a))
      val slurped = slurp(s)

      assert(slurped == Success(List(1, 1)), sum == 2)
    }
    test("Stream.throttleEnforce") {
      test("free elements") - {
        unsafeRun {
          Stream(1, 2, 3, 4)
            .throttleEnforce(0, Duration.Infinity)(_ => 0)
            .runCollect
            .map(l => assert(l == List(1, 2, 3, 4)))
        }
      }
      test("no bandwidth") - {
        unsafeRun {
          Stream(1, 2, 3, 4)
            .throttleEnforce(0, Duration.Infinity)(_ => 1)
            .runCollect
            .map(l => assert(l.isEmpty))
        }
      }
      test("throttle enforce short circuits") - {
        def delay(n: Int) = ZIO.sleep(5.milliseconds) *> UIO.succeed(n)

        unsafeRun {
          Stream(1, 2, 3, 4, 5)
            .mapM(delay)
            .throttleEnforce(2, Duration.Infinity)(_ => 1)
            .take(2)
            .runCollect
            .map(l => assert(l == List(1, 2)))
        }
      }
    }
    test("Stream.throttleShape") {
      test("free elements") - {
        unsafeRun {
          Stream(1, 2, 3, 4)
            .throttleShape(1, Duration.Infinity)(_ => 0)
            .runCollect
            .map(l => assert(l == List(1, 2, 3, 4)))
        }
      }
      test("throttle shape short circuits") - {
        unsafeRun {
          Stream(1, 2, 3, 4, 5)
            .throttleShape(2, Duration.Infinity)(_ => 1)
            .take(2)
            .runCollect
            .map(l => assert(l == List(1, 2)))
        }
      }
    }
    test("Stream.toQueue") - propTest {
      forAll { c: Chunk[Int] =>
        val s = Stream.fromChunk(c)
        val result = unsafeRunSync {
          s.toQueue(1000).use { queue: Queue[Take[Nothing, Int]] =>
            waitForSize(queue, c.length + 1) *> queue.takeAll
          }
        }
        result == Success(c.toSeq.toList.map(i => Take.Value(i)) :+ Take.End)
      }
    }
    test("Stream.transduce") {
      test("transduce") - {
        val s = Stream('1', '2', ',', '3', '4')
        val parser = ZSink.collectAllWhile[Char](_.isDigit).map(_.mkString.toInt) <* ZSink.collectAllWhile[Char](
          _ == ','
        )
        val transduced = s.transduce(parser)

        assert(slurp(transduced) == Success(List(12, 34)))
      }
      test("no remainder") - {
        val sink = Sink.fold(100) { (s, a: Int) =>
          if (a % 2 == 0)
            ZSink.Step.more(s + a)
          else
            ZSink.Step.done(s + a, Chunk.empty)
        }
        val transduced = ZStream(1, 2, 3, 4).transduce(sink)

        assert(slurp(transduced) == Success(List(101, 105, 104)))
      }
      test("with remainder") - {
        val sink = Sink.fold(0) { (s, a: Int) =>
          a match {
            case 1 => ZSink.Step.more(s + 100)
            case 2 => ZSink.Step.more(s + 100)
            case 3 => ZSink.Step.done(s + 3, Chunk(a + 1))
            case _ => ZSink.Step.done(s + 4, Chunk.empty)
          }
        }
        val transduced = ZStream(1, 2, 3).transduce(sink)

        assert(slurp(transduced) == Success(List(203, 4)))
      }
      test("with a sink that always signals more") - {
        val sink = Sink.fold(0) { (s, a: Int) =>
          ZSink.Step.more(s + a)
        }
        val transduced = ZStream(1, 2, 3).transduce(sink)

        assert(slurp(transduced) == Success(List(1 + 2 + 3)))
      }
      test("managed") - {
        final class TestSink(ref: Ref[Int]) extends ZSink[Any, Throwable, Int, Int, List[Int]] {
          override type State = List[Int]

          override def extract(state: List[Int]): ZIO[Any, Throwable, List[Int]] = ZIO.succeed(state)

          override def initial: ZIO[Any, Throwable, ZSink.Step[List[Int], Nothing]] = ZIO.succeed(ZSink.Step.more(Nil))

          override def step(state: List[Int], a: Int): ZIO[Any, Throwable, ZSink.Step[List[Int], Int]] =
            for {
              i <- ref.get
              _ <- if (i != 1000) IO.fail(new IllegalStateException(i.toString)) else IO.unit
            } yield ZSink.Step.done(List(a, a), Chunk.empty)
        }

        val stream = ZStream(1, 2, 3, 4)
        val test = for {
          resource <- Ref.make(0)
          sink     = ZManaged.make(resource.set(1000).const(new TestSink(resource)))(_ => resource.set(2000))
          result   <- stream.transduceManaged(sink).runCollect
          i        <- resource.get
          _        <- if (i != 2000) IO.fail(new IllegalStateException(i.toString)) else IO.unit
        } yield result
        assert(unsafeRunSync(test) == Success(List(List(1, 1), List(2, 2), List(3, 3), List(4, 4))))
      }
      test("propagate managed error") - {
        val fail = "I'm such a failure!"
        val sink = ZManaged.fail(fail)
        ZStream(1, 2, 3).transduceManaged(sink).runCollect.either.map(e => assert(e == Left(fail)))
      }
    }
    test("Stream.unfold") - {
      val s = Stream.unfold(0)(i => if (i < 10) Some((i, i + 1)) else None)
      assert(slurp(s) == Success((0 to 9).toList), slurp(s) == Success((0 to 9).toList))
    }
    test("Stream.unfoldM") - {
      val s = Stream.unfoldM(0)(i => if (i < 10) IO.succeed(Some((i, i + 1))) else IO.succeed(None))
      assert(slurp(s) == Success((0 to 9).toList), slurp(s) == Success((0 to 9).toList))
    }
    test("Stream.unTake") {
      test("unTake happy path") - {
        unsafeRun(
          Stream
            .range(0, 10)
            .toQueue[Nothing, Int](1)
            .use { q =>
              Stream.fromQueue(q).unTake.run(Sink.collectAll[Int])
            }
            .map(l => assert(l == (0 to 10).toList))
        )
      }
      test("unTake with error") - {
        val e = new RuntimeException("boom")
        val res = unsafeRunSync(
          (Stream.range(0, 10) ++ Stream.fail(e))
            .toQueue[Throwable, Int](1)
            .use { q =>
              Stream.fromQueue(q).unTake.run(Sink.collectAll[Int])
            }
        )
        assert(res == Failure(Cause.fail(e)))
      }
    }
    test("Stream zipping") {
      test("zipWith") - {
        val s1     = Stream(1, 2, 3)
        val s2     = Stream(1, 2)
        val zipped = s1.zipWith(s2)((a, b) => a.flatMap(a => b.map(a + _)))

        assert(slurp(zipped) == Success(List(2, 4)))
      }
      test("zipWithIndex") - propTest {
        forAll((s: Stream[String, Byte]) => slurp(s.zipWithIndex) == slurp(s).map(_.zipWithIndex))
      }
      test("zipWith ignore RHS") - {
        val s1     = Stream(1, 2, 3)
        val s2     = Stream(1, 2)
        val zipped = s1.zipWith(s2)((a, _) => a)

        assert(slurp(zipped) == Success(List(1, 2, 3)))
      }
      test("zipWith prioritizes failure") - {
        unsafeRun {
          Stream.never
            .zipWith(Stream.fail("Ouch"))((_, _) => None)
            .runCollect
            .either
            .map(l => assert(l == Left("Ouch")))
        }
      }
    }
  }
}
