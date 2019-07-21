package zio.duration

import java.time.{ Duration => JavaDuration }
import java.util.concurrent.TimeUnit

import utest._

import scala.concurrent.duration.{ Duration => ScalaDuration, FiniteDuration => ScalaFiniteDuration }

object DurationSpec extends TestSuite {
  override def tests: Tests = Tests {
    test("Make a Duration from positive nanos and check that") {
      test("The Duration is Finite") - assert(
        Duration.fromNanos(1).isInstanceOf[Duration.Finite]
      )
      test("Copy with a negative nanos returns Zero") - assert(
        Duration.fromNanos(1).asInstanceOf[Duration.Finite].copy(-1) == Duration.Zero
      )
      test("Multiplying with a negative factor returns Zero") - assert(
        Duration.fromNanos(1) * -1.0 == Duration.Zero
      )
      test("Its stdlib representation is correct") - assert(
        Duration.fromNanos(1234L).asScala == ScalaFiniteDuration(1234L, TimeUnit.NANOSECONDS)
      )
      test("Its JDK representation is correct") - assert(
        Duration.fromNanos(2345L).asJava == JavaDuration.ofNanos(2345L)
      )
      test("""It identifies as "zero"""") - assert(
        Duration.fromNanos(0L).isZero
      )
      test("Creating it with a j.u.c.TimeUnit is identical") - assert(
        Duration(12L, TimeUnit.NANOSECONDS) == Duration.fromNanos(12L)
      )
      test("It knows its length in ns") - assert(
        Duration.fromNanos(123L).toNanos == 123L
      )
      test("It knows its length in ms") - assert(
        Duration.fromNanos(123000000L).toMillis == 123L
      )
      test("max(1 ns, 2 ns) is 2 ns") - assert(
        Duration.fromNanos(1L).max(Duration.fromNanos(2L)) == Duration.fromNanos(2L)
      )
      test("min(1 ns, 2 ns) is 1 ns") - assert(
        Duration.fromNanos(1L).min(Duration.fromNanos(2L)) == Duration.fromNanos(1L)
      )
      test("max(2 ns, 1 ns) is 2 ns") - assert(
        Duration.fromNanos(2L).max(Duration.fromNanos(1L)) == Duration.fromNanos(2L)
      )
      test("min(2 ns, 1 ns) is 1 ns") - assert(
        Duration.fromNanos(2L).min(Duration.fromNanos(1L)) == Duration.fromNanos(1L)
      )
      test("10 ns + 20 ns = 30 ns") - assert(
        Duration.fromNanos(10L) + Duration.fromNanos(20L) == Duration.fromNanos(30L)
      )
      test("10 ns * NaN = Infinity") - assert(
        Duration.fromNanos(10L) * Double.NaN == Duration.Infinity
      )
      test("10 ns compared to Infinity is -1") - assert(
        Duration.fromNanos(10L).compare(Duration.Infinity) == -1
      )
      test("10 ns compared to 10 ns is 0") - assert(
        Duration.fromNanos(10L).compare(Duration.fromNanos(10L)) == 0
      )
      test("+ with positive overflow results in Infinity") - assert(
        Duration.fromNanos(Long.MaxValue - 1) + Duration.fromNanos(42) == Duration.Infinity
      )
      test("* with negative duration results in zero") - assert(
        Duration.fromNanos(42) * -7 == Duration.Zero
      )
      test("* with overflow to positive results in Infinity") - assert(
        Duration.fromNanos(Long.MaxValue) * 3 == Duration.Infinity
      )
      test("* with overflow to negative results in Infinity") - assert(
        Duration.fromNanos(Long.MaxValue) * 2 == Duration.Infinity
      )
      test("Folding picks up the correct value") - assert(
        Duration.fromNanos(Long.MaxValue).fold("Infinity", _ => "Finite") == "Finite"
      )
    }
    test("Make a Duration from negative nanos and check that") {
      test("The Duration is Zero") - assert(Duration.fromNanos(-1) == Duration.Zero)
    }
    test("Take Infinity and check that") {
      test("It returns -1 milliseconds") - assert(
        Duration.Infinity.toMillis == Long.MaxValue / 1000000
      )
      test("It returns -1 nanoseconds") - assert(
        Duration.Infinity.toNanos == Long.MaxValue
      )
      test("Infinity + Infinity = Infinity") - assert(
        Duration.Infinity + Duration.Infinity == Duration.Infinity
      )
      test("Infinity + 1 ns = Infinity") - assert(
        Duration.Infinity + Duration.fromNanos(1L) == Duration.Infinity
      )
      test("1 ns + Infinity = Infinity") - assert(
        Duration.fromNanos(1L) + Duration.Infinity == Duration.Infinity
      )
      test("Infinity * 10 = Infinity") - assert(
        Duration.Infinity * 10.0 == Duration.Infinity
      )
      test("Infinity compared to Infinity is 0") - assert(
        Duration.Infinity.compare(Duration.Infinity) == 0
      )
      test("Infinity compared to 1 ns is 1") - assert(
        Duration.Infinity.compare(Duration.fromNanos(1L)) == 1
      )
      test("Infinity is not zero") - assert(
        !Duration.Infinity.isZero
      )
      test("It converts into the infinite s.c.d.Duration") - assert(
        Duration.Infinity.asScala == ScalaDuration.Inf
      )
      test("It converts into a Long.MaxValue second-long JDK Duration") - assert(
        Duration.Infinity.asJava == JavaDuration.ofSeconds(Long.MaxValue)
      )
      test("Folding picks up the correct value") - assert(
        Duration.Infinity.fold("Infinity", _ => "Finite") == "Infinity"
      )
      test("Infinity * -10 = Zero") - assert(
        Duration.Infinity * -10 == Duration.Zero
      )
    }
    test("Make a Scala stdlib s.c.d.Duration and check that") {
      test("A negative s.c.d.Duration converts to Zero") - assert(
        Duration.fromScala(ScalaDuration(-1L, TimeUnit.NANOSECONDS)) == Duration.Zero
      )
      test("The infinite s.c.d.Duration converts to Infinity") - assert(
        Duration.fromScala(ScalaDuration.Inf) == Duration.Infinity
      )
      test("A positive s.c.d.Duration converts to a Finite") - assert(
        Duration.fromScala(ScalaDuration(1L, TimeUnit.NANOSECONDS)) == Duration.fromNanos(1L)
      )
    }
    test("Make a Java stdlib j.t.Duration and check that") {
      test("A negative j.t.Duration converts to Zero") - assert(
        Duration.fromJava(JavaDuration.ofNanos(-1L)) == Duration.Zero
      )
      test("A Long.MaxValue second j.t.Duration converts to Infinity") - assert(
        Duration.fromJava(JavaDuration.ofSeconds(Long.MaxValue)) == Duration.Infinity
      )
      test("A nano-adjusted Long.MaxValue second j.t.Duration converts to Infinity") - assert(
        Duration.fromJava(JavaDuration.ofSeconds(Long.MaxValue, 1L)) == Duration.Infinity
      )
      test("A j.t.Duration constructed from Infinity converts to Infinity") - assert(
        Duration.fromJava(Duration.Infinity.asJava) == Duration.Infinity
      )
      test("A Long.MaxValue - 1 second j.t.Duration converts to Infinity") - assert(
        Duration.fromJava(JavaDuration.ofSeconds(Long.MaxValue - 1)) == Duration.Infinity
      )
      test("A +ve j.t.Duration whose nano conversion overflows converts to Infinity") - assert(
        Duration.fromJava(JavaDuration.ofNanos(Long.MaxValue).plus(JavaDuration.ofNanos(1L))) == Duration.Infinity
      )
      test("A -ve j.t.Duration whose nano conversion overflows converts to Zero") - assert(
        Duration.fromJava(JavaDuration.ofNanos(Long.MinValue).minus(JavaDuration.ofNanos(1L))) == Duration.Zero
      )
      test("A positive j.t.Duration converts to a Finite") - assert(
        Duration.fromJava(JavaDuration.ofNanos(1L)) == Duration.fromNanos(1L)
      )
    }
    test("Check multiplication with finite durations") {
      test("Zero multiplied with zero") - assert(
        Duration.Zero * 0 == Duration.Zero
      )
      test("Zero multiplied with one") - assert(
        Duration.Zero * 1 == Duration.Zero
      )
      test("One second multiplied with 60") - assert(
        Duration(1, TimeUnit.SECONDS) * 60 == Duration(1, TimeUnit.MINUTES)
      )
    }
  }
}
