package zio.duration

import utest._

object DurationSyntaxSpec extends TestSuite {
  override def tests: Tests = Tests {
    test("Long") {
      test("1L.nano         = fromNanos(1L)") - assert(1L.nano == Duration.fromNanos(1L))
      test("2L.nanos        = fromNanos(2L)") - assert(2L.nanos == Duration.fromNanos(2L))
      test("1L.nanosecond   = fromNanos(1L)") - assert(1L.nanosecond == Duration.fromNanos(1L))
      test("2L.nanoseconds  = fromNanos(2L)") - assert(2L.nanoseconds == Duration.fromNanos(2L))
      test("1L.micro        = fromNanos(1000L)") - assert(1L.micro == Duration.fromNanos(1000L))
      test("2L.micros       = fromNanos(2000L)") - assert(2L.micros == Duration.fromNanos(2000L))
      test("1L.microsecond  = fromNanos(1000L)") - assert(1L.microsecond == Duration.fromNanos(1000L))
      test("2L.microseconds = fromNanos(2000L)") - assert(2L.microseconds == Duration.fromNanos(2000L))
      test("1L.milli        = fromNanos(1000000L)") - assert(1L.milli == Duration.fromNanos(1000000L))
      test("2L.millis       = fromNanos(2000000L)") - assert(2L.millis == Duration.fromNanos(2000000L))
      test("1L.millisecond  = fromNanos(1000000L)") - assert(1L.millisecond == Duration.fromNanos(1000000L))
      test("2L.milliseconds = fromNanos(2000000L)") - assert(2L.milliseconds == Duration.fromNanos(2000000L))
      test("1L.second       = fromNanos(1000000000L)") - assert(1L.second == Duration.fromNanos(1000000000L))
      test("2L.seconds      = fromNanos(2000000000L)") - assert(2L.seconds == Duration.fromNanos(2000000000L))
      test("1L.minute       = fromNanos(60000000000L)") - assert(1L.minute == Duration.fromNanos(60000000000L))
      test("2L.minutes      = fromNanos(120000000000L)") - assert(2L.minutes == Duration.fromNanos(120000000000L))
      test("1L.hour         = fromNanos(3600000000000L)") - assert(1L.hour == Duration.fromNanos(3600000000000L))
      test("2L.hours        = fromNanos(7200000000000L)") - assert(2L.hours == Duration.fromNanos(7200000000000L))
      test("1L.day          = fromNanos(86400000000000L)") - assert(1L.day == Duration.fromNanos(86400000000000L))
      test("2L.days         = fromNanos(172800000000000L)") - assert(2L.days == Duration.fromNanos(172800000000000L))
    }
    test("Int") {
      test("1.nano         = fromNanos(1L)") - assert(1.nano == Duration.fromNanos(1L))
      test("2.nanos        = fromNanos(2L)") - assert(2.nanos == Duration.fromNanos(2L))
      test("1.nanosecond   = fromNanos(1L)") - assert(1.nanosecond == Duration.fromNanos(1L))
      test("2.nanoseconds  = fromNanos(2L)") - assert(2.nanos == Duration.fromNanos(2L))
      test("1.micro        = fromNanos(1000L)") - assert(1.micro == Duration.fromNanos(1000L))
      test("2.micros       = fromNanos(2000L)") - assert(2.micros == Duration.fromNanos(2000L))
      test("1.microsecond  = fromNanos(1000L)") - assert(1.microsecond == Duration.fromNanos(1000L))
      test("2.microseconds = fromNanos(2000L)") - assert(2.microseconds == Duration.fromNanos(2000L))
      test("1.milli        = fromNanos(1000000L)") - assert(1.milli == Duration.fromNanos(1000000L))
      test("2.millis       = fromNanos(2000000L)") - assert(2.millis == Duration.fromNanos(2000000L))
      test("1.millisecond  = fromNanos(1000000L)") - assert(1.millisecond == Duration.fromNanos(1000000L))
      test("2.milliseconds = fromNanos(2000000L)") - assert(2.milliseconds == Duration.fromNanos(2000000L))
      test("1.second       = fromNanos(1000000000L)") - assert(1.second == Duration.fromNanos(1000000000L))
      test("2.seconds      = fromNanos(2000000000L)") - assert(2.seconds == Duration.fromNanos(2000000000L))
      test("1.minute       = fromNanos(60000000000L)") - assert(1.minute == Duration.fromNanos(60000000000L))
      test("2.minutes      = fromNanos(120000000000L)") - assert(2.minutes == Duration.fromNanos(120000000000L))
      test("1.hour         = fromNanos(3600000000000L)") - assert(1.hour == Duration.fromNanos(3600000000000L))
      test("2.hours        = fromNanos(7200000000000L)") - assert(2.hours == Duration.fromNanos(7200000000000L))
      test("1.day          = fromNanos(86400000000000L)") - assert(1.day == Duration.fromNanos(86400000000000L))
      test("2.days         = fromNanos(76800000000000L)") - assert(2.days == Duration.fromNanos(172800000000000L))
    }
  }
}
