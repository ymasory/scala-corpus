/*
 * Copyright 2001-2008 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scalatest.concurrent

import org.scalatest._
import org.scalatest.events._
import java.util.concurrent.atomic.AtomicReference
import _root_.java.util.concurrent.Callable

/**
 * Trait that provides each test with access to a new <code>Conductor</code> 
 * via methods.
 *
 * <p>
 * Here's an example of the use of this trait to test the <code>ArrayBlockingQueue</code>
 * concurrency abstraction from <code>java.util.concurrent</code>:
 * </p>
 *
 * <pre>
 * import org.scalatest.FunSuite
 * import org.scalatest.concurrent.ConductorMethods
 * import org.scalatest.matchers.ShouldMatchers
 * import java.util.concurrent.ArrayBlockingQueue
 *
 * class ArrayBlockingQueueSuite extends FunSuite with ConductorMethods with ShouldMatchers {
 * 
 *   test("calling put on a full queue blocks the producer thread") {
 *
 *     val buf = new ArrayBlockingQueue[Int](1)
 * 
 *     thread("producer") {
 *       buf put 42
 *       buf put 17
 *       beat should be (1)
 *     }
 * 
 *     thread("consumer") {
 *       waitForBeat(1)
 *       buf.take should be (42)
 *       buf.take should be (17)
 *     }
 * 
 *     whenFinished {
 *       buf should be ('empty)
 *     }
 *   }
 *
 *   test("calling take on an empty queue blocks the consumer thread") {
 *
 *     val buf = new ArrayBlockingQueue[Int](1)
 *
 *     thread("producer") {
 *       waitForBeat(1)
 *       buf put 42
 *       buf put 17
 *     }
 *
 *     thread("consumer") {
 *       buf.take should be (42)
 *       buf.take should be (17)
 *       beat should be (1)
 *     }
 *
 *     whenFinished {
 *       buf should be ('empty)
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * For an explanation of how these tests work, see the documentation for <a href="Conductor.html"><code>Conductor</code></a>.
 * </p>
 *
 * @author Josh Cough
 * @author Bill Venners
 */
trait ConductorMethods extends AbstractSuite { this: Suite =>

  private val conductor = new AtomicReference[Conductor]()

  /**
   * Create a new thread that will execute the given function.
   * If the test is started, then the thread will run the function immediately.
   * If it is not yet started, the Thread will wait to run the function until
   * all threads are up and ready to go.
   * @param f the function to be executed by the thread
   */
  protected def thread[T](f: => T): Thread = conductor.get.thread{ f }

  /**
   * Create a new thread that will execute the given function.
   * If the test is started, then the thread will run the function immediately.
   * If it is not yet started, the Thread will wait to run the function until
   * all threads are up and ready to go.
   * @param name the name of the thread
   * @param f the function to be executed by the thread
   */
  protected def thread[T](name: String)(f: => T): Thread = conductor.get.thread(name){ f }

  /*
   * Create a new thread that will execute the given Runnable
   * @param runnable the Runnable to be executed by the thread
   */
  // def thread[T](runnable: Runnable): Thread = conductor.get.thread(runnable)

  /*
   * Create a new thread that will execute the given Runnable
   * @param name the name of the thread
   * @param runnable the Runnable to be executed by the thread
   */
  // def thread[T](name: String, runnable: Runnable): Thread = conductor.get.thread(name,runnable)

  /*
   * Create a new thread that will execute the given Callable
   * @param callable the Callable to be executed by the thread
   */
  // def thread[T](callable: Callable[T]): Thread = conductor.get.thread(callable)

  /*
   * Create a new thread that will execute the given Callable
   * @param name the name of the thread
   * @param callable the Callable to be executed by the thread
   */
  // def thread[T](name: String, callable: Callable[T]): Thread = conductor.get.thread(name,callable)

  /**
   * Force the current thread to block until the thread clock reaches the
   * specified value, at which point the current thread is unblocked.
   * @param c the tick value to wait for
   */
  protected def waitForBeat(beat:Int) = conductor.get.waitForBeat(beat)

  /**
   * Run the passed function, ensuring the clock does not advance while the function is running
   * (has not yet returned or thrown an exception).
   */
  protected def withConductorFrozen[T](f: => T) = conductor.get.withConductorFrozen(f)

  /**
   * Check if the clock has been frozen by any threads. (The only way a thread
   * can freeze the clock is by calling withClockFrozen.)
   */
  protected def isConductorFrozen: Boolean = conductor.get.isConductorFrozen

  /**
   * Gets the current value of the clock. Primarily useful in assert statements.
   * @return the current tick value
   */
  protected def beat = conductor.get.beat

  /**
   * Register a function to be executed after the simulation has finished.
   */
  protected def whenFinished(fun: => Unit) = conductor.get.whenFinished { fun }

  /**
   * Creates and initializes a private instance variable with a new Conductor,
   * ensuring it is visible to any thread, invokes the passed test function,
   * and invokes <code>conduct</code> on the <code>Conductor</code>, if it
   * was not already invoked by the test.
   */
  override def withFixture(test: NoArgTest) {
    conductor.compareAndSet(conductor.get, new Conductor)
    test()
    if (!conductor.get.conductingHasBegun)
      conductor.get.conduct()
  }
}
