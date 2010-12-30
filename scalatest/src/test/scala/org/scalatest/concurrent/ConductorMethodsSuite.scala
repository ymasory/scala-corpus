package org.scalatest.concurrent

import org.scalatest._
import matchers.ShouldMatchers
import _root_.java.util.concurrent.{Callable, CountDownLatch}
import Thread.State._

class ConductorMethodsSuite extends FunSuite with ConductorMethods with ShouldMatchers {

  test("metronome order") {

    val volatileString = new VolatileString
    import volatileString._

    thread("t1") {
      waitForBeat(1)
      s = s + "A"

      waitForBeat(3)
      s = s + "C"

      waitForBeat(6)
      s = s + "F"
    }

    thread("t2") {
      waitForBeat(2)
      s = s + "B"

      waitForBeat(5)
      s = s + "E"

      waitForBeat(8)
      s = s + "H"
    }

    thread("t3") {
      waitForBeat(4)
      s = s + "D"

      waitForBeat(7)
      s = s + "G"

      waitForBeat(9)
      s = s + "I"
    }

    whenFinished {
      s should be ("ABCDEFGHI") // "Threads were not called in correct order"
    }
  }

  test("wait for tick advances when threads are blocked") {
    var c = new CountDownLatch(3)

    thread {
      c.countDown()
      c.await()
    }

    thread {
      c.countDown()
      c.await()
    }

    thread {
      waitForBeat(1)
      c.getCount should be (1) // TODO: Got an error: 2 was not equal to 1
      waitForBeat(2) // advances quickly
      c.getCount should be (1)
      c.countDown()
    }

    whenFinished {
      c.getCount() should be (0)
    }
  }

  // TODO: t1.getState should (be(WAITING) or be(BLOCKED)) failed with:
  // RUNNABLE was not equal to WAITING, and RUNNABLE was not equal to BLOCKED
  // A thread in a wait set is allowed to show up as RUNNABLE. It could be spin waiting
  // or just wake up temporarily. So even though this fails very occasionally, it probably
  // doesn't indicate a bug. (It failed on the Azul server after I cleaned up the bugs
  // in Conductor.)
  test("wait for beat blocks thread") {

    val t1 = thread { waitForBeat(2) }

    thread {
      waitForBeat(1)
      t1.getState should (be (WAITING) or be (BLOCKED))
    }
  }

  test("thread terminates before finish called") {

    val t1 = thread {1 should be (1)}
    val t2 = thread {1 should be (1)}

    whenFinished {
      t1.getState should be (TERMINATED)
      t2.getState should be (TERMINATED)
    }
  }

  test("two thread calls return threads that both are in the same thread group") {

    val t1 = thread {waitForBeat(2)}
    val t2 = thread {waitForBeat(2)}

    thread {
      waitForBeat(1)
      t1.getThreadGroup should be (t2.getThreadGroup)
    }
  }

  test("if a thread call is nested inside another thread call, both threads are in the same thread group") {
    thread {
      val t2 = thread {waitForBeat(2)}
      waitForBeat(1)
      t2.getThreadGroup should be (currentThread.getThreadGroup)
    }
  }

  test("whenFinished can only be called by thread that created Conductor.") {
    thread {
      intercept[NotAllowedException] {
        whenFinished {1 should be (1)}
      }.getMessage should be ("whenFinished can only be called by the thread that created Conductor.")
    }
    whenFinished {1 should be (1)}
  }

  test("nested thread calls result in a running thread that is allowed to execute immediately") (pending)

  /////////////////////////////////////////////////

  test("the test still executes (because " +
         "ConductorMethods calls conduct)") (pending)


  test("calling withConductorFrozen by two threads or twice will not screw things up. In other words, " +
          "whether or not the conductor is frozen should probably be a counting semaphor, not a flag") (pending)

  test("whenFinished throws an IllegalStateException if it is invoked during the running or" +
          "defunct phases,") (pending)

  test("conduct throws an IllegalStateException if it is invoked more than once.") (pending)
}
