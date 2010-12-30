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

/**
 * Trait whose instances can indicate whether a stop has been requested. This is passed in
 * to the <code>run</code> method of <code>Suite</code>, so that running suites of tests can be
 * requested to stop early.
 *
 * @author Bill Venners
 */
trait Stopper extends (() => Boolean) {

  /**
   * Indicates whether a stop has been requested.  Call this method
   * to determine whether a running test should stop. The <code>run</code> method of any <code>Suite</code>, or
   * code invoked by <code>run</code>, should periodically check the
   * stop requested function. If <code>true</code>,
   * the <code>run</code> method should interrupt its work and simply return.
   */
  override def apply() = false
}
// One question people will have is do I make this a val or a def in the supertype.
// A val can override a def. Can it be the other way around? How does he implement
// this?

/*
   Could make this a function too. Would simply be () => Boolean. Could name the parameter stopRequested
  
   Then the code would be:

   if (stopRequested()) {
     // bla bla bla
   }

   Instead of:

   if (stopper.stopRequested) {
     // bla bla bla
   }

   Could call it StopRequestedFunction instead of Stopper
   stopRequested: StopRequestedFunction

   Or could just not give it a name so they'd write:
   stopRequested: () => Boolean

   Blech. Could also use StopRequestedFun
   stopRequested: StopRequestedFun

  Or just, StopRequested
  stopRequested: StopRequested

  StopRequested, Distribute, Report (hmm. deprecation is a problem here), Filter
  StopRequestedFun, DistributeFun, ReportFun, FilterFun

  Honestly, I kind of like: StopRequested, Distribute, Filter, and Report (problem with these latter two is they pass as nouns too, and in the case of Report that's misleading)

  StopRequestedFunction, DistributeFunction, ReportFunction, FilterFunction
  testName: Option[String], report: ReportFunction, stopRequested: StopRequestedFunction, filter: FilterFunction, configMap: Map[String, Set[String]], distribute: Option[DistributeFunction]

  I think that latter is the most clear.
*/



