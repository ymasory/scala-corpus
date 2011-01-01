package blueeyes.health.metrics

import org.scalatest.matchers.MustMatchers
import org.scalatest.Spec
import blueeyes.health.time.Duration
import blueeyes.util.Future

class TimerTest extends Spec with MustMatchers {
  val precision = 5.0 // milliseconds

  describe("timing an event") {
    it("returns the event's value") {
      val timer = new Timer
      timer.time { 1 + 1 } must equal(2)
    }

    it("records the duration of the event") {
      val timer = new Timer
      timer.time { Thread.sleep(10) }
      timer.mean.ms.value must not be(0.0)
    }

    it("records the duration of the event specified by future") {
      val timer  = new Timer
      val future = new Future[Unit]()
      timer.time(Future.async[Unit]{Thread.sleep(10); ()} )
      Thread.sleep(20)
      timer.mean.ms.value must not be(0.0)
    }

    it("records the existence of the event") {
      val timer = new Timer
      timer.time { Thread.sleep(10) }

      timer.count must be(1)
    }
  }

  describe("a blank timer") {
    val timer = new Timer

    it("has a max of zero") {
      timer.max.ms.value must be(0.0 plusOrMinus precision)
    }

    it("has a min of zero") {
      timer.min.ms.value must be(0.0 plusOrMinus precision)
    }

    it("has a mean of zero") {
      timer.mean.ms.value must be(0.0 plusOrMinus precision)
    }

    it("has a standard deviation of zero") {
      timer.standardDeviation.ms.value must be(0.0 plusOrMinus precision)
    }

    it("has a count of zero") {
      timer.count must be (0)
    }
  }

  describe("timing a series of events") {
    val timer = new Timer
    timer ++= List(
      Duration.milliseconds(10),
      Duration.milliseconds(20),
      Duration.milliseconds(20),
      Duration.milliseconds(30),
      Duration.milliseconds(40)
    )

    it("calculates the maximum duration") {
      timer.max.ms.value must be(40.0 plusOrMinus precision)
    }

    it("calculates the minimum duration") {
      timer.min.ms.value must be(10.0 plusOrMinus precision)
    }

    it("calculates the mean") {
      timer.mean.ms.value must be(24.0 plusOrMinus precision)
    }

    it("calculates the standard deviation") {
      timer.standardDeviation.ms.value must be(11.4 plusOrMinus precision)
    }

    it("records the count") {
      timer.count must be (5)
    }
  }

  describe("timing crazy-variant values") {
    val timer = new Timer
    timer ++= List(
      Duration.milliseconds(Long.MaxValue),
      Duration.milliseconds(0)
    )

    it("calculates the standard deviation without overflowing") {
      timer.standardDeviation.ms.value must be(6.521908912666392E12 plusOrMinus 1E3)
    }
  }
}