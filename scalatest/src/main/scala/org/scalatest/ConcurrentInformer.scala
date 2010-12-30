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
package org.scalatest

import events.NameInfo
import java.util.concurrent.atomic.AtomicReference

/*
 This is used by Suite and test informers created as tests run, which therefore have
 populated defined NameInfos. These informers are returned by info in FunSuite and Spec,
 or passed to test methods that take an Informer in Suite, for example. If called by the
 thread that constructed them, which is the thread that was executing the suite and the tests
 inside the suite, then that NameInfo should be propagated. However, if a test starts other
 threads for a multi-threaded test, and those threads apply the Informer, then the NameInfo
 should *not* be propagated, because otherwise it could become very confusing to figure out
 what came from where in the report. Threads started by the test could outlast the thread
 that was running the test, for example. There will be a thread-name, so they can use that
 to figure out who sent what. And threads that call these informers will share a Tracker with
 the thread that was running the tests, so they should be ordered close together after
 sorting by Ordinal. But that's it. NameInfo only goes out when the thread running a test
 or suite applies the Informer.

 This in turn means that a reporter may get hit by multiple threads sending InfoProvided
 messages. If run with the Runner, that will be OK, because DispatchReporter will be in front
 serializing events with its actor. If run() is invoked directly on a suite instance, such as
 from the Scala interpretter, then it may not work. I think I may just say that when running
 from the interpreter, say with run(), you may get interleaved output. This would only happen
 when doing a multi-threaded test that starts threads that calls informer methods, likely a
 rare case. Also, in that case I think it is reasonable to say you may get interleaved output
 in the interpreter, so if you don't like that, use the Runner.
*/
private[scalatest] abstract class ConcurrentInformer(nameInfo: NameInfo) extends Informer {

  private final val atomic = new AtomicReference[(Thread, Option[NameInfo])](Thread.currentThread, Some(nameInfo))

  def nameInfoForCurrentThread: Option[NameInfo] = {
    val (constructingThread, nameInfo) = atomic.get
    if (Thread.currentThread == constructingThread) nameInfo else None
  }
}