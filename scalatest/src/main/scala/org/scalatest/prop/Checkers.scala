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
package org.scalatest.prop

import org.scalatest._
import org.scalatest.Suite
import org.scalacheck.Arbitrary
import org.scalacheck.Shrink
import org.scalacheck.Pretty
import org.scalacheck.Arg
import org.scalacheck.Prop
import org.scalacheck.Test
import org.scalatest.StackDepthExceptionHelper.getStackDepth

/**
 * Trait that contains several &#8220;check&#8221; methods that perform ScalaCheck property checks.
 * If ScalaCheck finds a test case for which a property doesn't hold, the problem will be reported as a ScalaTest test failure.
 * 
 * <p>
 * To use ScalaCheck, you specify properties and, in some cases, generators that generate test data. You need not always
 * create generators, because ScalaCheck provides many default generators for you that can be used in many situations.
 * ScalaCheck will use the generators to generate test data and with that data run tests that check that the property holds.
 * Property-based tests can, therefore, give you a lot more testing for a lot less code than assertion-based tests.
 * Here's an example of using ScalaCheck from a <code>JUnitSuite</code>:
 * </p>
 * <pre>
 * import org.scalatest.junit.JUnitSuite
 * import org.scalatest.prop.Checkers
 * import org.scalacheck.Arbitrary._
 * import org.scalacheck.Prop._
 *
 * class MySuite extends JUnitSuite with Checkers {
 *   @Test
 *   def testConcat() {
 *     check((a: List[Int], b: List[Int]) => a.size + b.size == (a ::: b).size)
 *   }
 * }
 * </pre>
 * <p>
 * The <code>check</code> method, defined in <code>Checkers</code>, makes it easy to write property-based tests inside
 * ScalaTest, JUnit, and TestNG test suites. This example specifies a property that <code>List</code>'s <code>:::</code> method
 * should obey. ScalaCheck properties are expressed as function values that take the required
 * test data as parameters. ScalaCheck will generate test data using generators and 
repeatedly pass generated data to the function. In this case, the test data is composed of integer lists named <code>a</code> and <code>b</code>.
 * Inside the body of the function, you see:
 * </p>
 * <pre>
 * a.size + b.size == (a ::: b).size
 * </pre>
 * <p>
 * The property in this case is a <code>Boolean</code> expression that will yield true if the size of the concatenated list is equal
 * to the size of each individual list added together. With this small amount
 * of code, ScalaCheck will generate possibly hundreds of value pairs for <code>a</code> and <code>b</code> and test each pair, looking for
 * a pair of integers for which the property doesn't hold. If the property holds true for every value ScalaCheck tries,
 * <code>check</code> returns normally. Otherwise, <code>check</code> will complete abruptly with a <code>TestFailedException</code> that
 * contains information about the failure, including the values that cause the property to be false.
 * </p>
 *
 * <p>
 * For more information on using ScalaCheck properties, see the documentation for ScalaCheck, which is available
 * from <a href="http://code.google.com/p/scalacheck/">http://code.google.com/p/scalacheck/</a>.
 * </p>
 *
 * <p>
 * To execute a suite that mixes in <code>Checkers</code> with ScalaTest's <code>Runner</code>, you must include ScalaCheck's jar file on the class path or runpath.
 * This version of <code>Checkers</code> was tested with ScalaCheck version 1.1.1. This trait must
 * be mixed into a ScalaTest <code>Suite</code>, because its self type is <code>org.scalatest.Suite</code>.
 * </p>
 *
 * @author Bill Venners
 */
trait Checkers {

  this: Suite =>

  /**
   * Convert the passed 1-arg function into a property, and check it.
   *
   * @param f the function to be converted into a property and checked
   * @throws TestFailedException if a test case is discovered for which the property doesn't hold.
   */
  def check[A1,P](f: A1 => P)
    (implicit
      p: P => Prop,
      a1: Arbitrary[A1], s1: Shrink[A1], pp1: A1 => Pretty
    ) {
    check(Prop.forAll(f)(p, a1, s1, pp1))
  }

  /**
   * Convert the passed 2-arg function into a property, and check it.
   *
   * @param f the function to be converted into a property and checked
   * @throws TestFailedException if a test case is discovered for which the property doesn't hold.
   */
  def check[A1,A2,P](f: (A1,A2) => P)
    (implicit
      p: P => Prop,
      a1: Arbitrary[A1], s1: Shrink[A1], pp1: A1 => Pretty,
      a2: Arbitrary[A2], s2: Shrink[A2], pp2: A2 => Pretty
    ) {
    check(Prop.forAll(f)(p, a1, s1, pp1, a2, s2, pp2))
  }

  /**
   * Convert the passed 3-arg function into a property, and check it.
   *
   * @param f the function to be converted into a property and checked
   * @throws TestFailedException if a test case is discovered for which the property doesn't hold.
   */
  def check[A1,A2,A3,P](f: (A1,A2,A3) => P)
    (implicit
      p: P => Prop,
      a1: Arbitrary[A1], s1: Shrink[A1], pp1: A1 => Pretty,
      a2: Arbitrary[A2], s2: Shrink[A2], pp2: A2 => Pretty,
      a3: Arbitrary[A3], s3: Shrink[A3], pp3: A3 => Pretty
    ) {
    check(Prop.forAll(f)(p, a1, s1, pp1, a2, s2, pp2, a3, s3, pp3))
  }

  /**
   * Convert the passed 4-arg function into a property, and check it.
   *
   * @param f the function to be converted into a property and checked
   * @throws TestFailedException if a test case is discovered for which the property doesn't hold.
   */
  def check[A1,A2,A3,A4,P](f: (A1,A2,A3,A4) => P)
    (implicit
      p: P => Prop,
      a1: Arbitrary[A1], s1: Shrink[A1], pp1: A1 => Pretty,
      a2: Arbitrary[A2], s2: Shrink[A2], pp2: A2 => Pretty,
      a3: Arbitrary[A3], s3: Shrink[A3], pp3: A3 => Pretty,
      a4: Arbitrary[A4], s4: Shrink[A4], pp4: A4 => Pretty
    ) {
    check(Prop.forAll(f)(p, a1, s1, pp1, a2, s2, pp2, a3, s3, pp3, a4, s4, pp4))
  }

  /**
   * Convert the passed 5-arg function into a property, and check it.
   *
   * @param f the function to be converted into a property and checked
   * @throws TestFailedException if a test case is discovered for which the property doesn't hold.
   */
  def check[A1,A2,A3,A4,A5,P](f: (A1,A2,A3,A4,A5) => P)
    (implicit
      p: P => Prop,
      a1: Arbitrary[A1], s1: Shrink[A1], pp1: A1 => Pretty,
      a2: Arbitrary[A2], s2: Shrink[A2], pp2: A2 => Pretty,
      a3: Arbitrary[A3], s3: Shrink[A3], pp3: A3 => Pretty,
      a4: Arbitrary[A4], s4: Shrink[A4], pp4: A4 => Pretty,
      a5: Arbitrary[A5], s5: Shrink[A5], pp5: A5 => Pretty
    ) {
    check(Prop.forAll(f)(p, a1, s1, pp1, a2, s2, pp2, a3, s3, pp3, a4, s4, pp4, a5, s5, pp5))
  }

  /**
   * Convert the passed 6-arg function into a property, and check it.
   *
   * @param f the function to be converted into a property and checked
   * @throws TestFailedException if a test case is discovered for which the property doesn't hold.
   */
  def check[A1,A2,A3,A4,A5,A6,P](f: (A1,A2,A3,A4,A5,A6) => P)
    (implicit
      p: P => Prop,
      a1: Arbitrary[A1], s1: Shrink[A1], pp1: A1 => Pretty,
      a2: Arbitrary[A2], s2: Shrink[A2], pp2: A2 => Pretty,
      a3: Arbitrary[A3], s3: Shrink[A3], pp3: A3 => Pretty,
      a4: Arbitrary[A4], s4: Shrink[A4], pp4: A4 => Pretty,
      a5: Arbitrary[A5], s5: Shrink[A5], pp5: A5 => Pretty,
      a6: Arbitrary[A6], s6: Shrink[A6], pp6: A6 => Pretty
    ) {
    check(Prop.forAll(f)(p, a1, s1, pp1, a2, s2, pp2, a3, s3, pp3, a4, s4, pp4, a5, s5, pp5, a6, s6, pp6))
  }

  /**
   * Check a property with the given testing parameters.
   *
   * @param p the property to check
   * @param prms the test parameters
   * @throws TestFailedException if a test case is discovered for which the property doesn't hold.
   */
  def check(p: Prop, prms: Test.Params) {

    val result = Test.check(prms, p)
    if (!result.passed) {

      result.status match {

        case Test.Proved(args) =>

          val (args, labels) = argsAndLabels(result)
          throw new PropertyTestFailedException(
            prettyTestStats(result),
            None,
            getStackDepth("ScalaCheck.scala", "check"),
            FailureMessages("propertyProved"),
            args,
            labels
          )

        case Test.Passed => // Should never get here, because this is executed only if !result.passed. Better to refactor this away

          val (args, labels) = argsAndLabels(result)
          throw new PropertyTestFailedException(
            prettyTestStats(result),
            None,
            getStackDepth("ScalaCheck.scala", "check"),
            FailureMessages("propertyPassed", result.succeeded),
            args,
            labels
          )

        case Test.Exhausted =>

          val (args, labels) = argsAndLabels(result)
          throw new PropertyTestFailedException(
            prettyTestStats(result),
            None,
            getStackDepth("ScalaCheck.scala", "check"),
            FailureMessages("propertyExhausted", result.succeeded, result.discarded),
            args,
            labels
          )

        case Test.Failed(scalaCheckArgs, scalaCheckLabels) =>

          val (args, labels) = argsAndLabels(result)
          throw new PropertyTestFailedException(
            prettyTestStats(result),
            None,
            getStackDepth("ScalaCheck.scala", "check"),
            FailureMessages("propertyFailed", result.succeeded),
            args,
            labels
          )

        case Test.PropException(scalaCheckArgs, e, scalaCheckLabels) =>
          val (args, labels) = argsAndLabels(result)
          throw new PropertyTestFailedException(
            prettyTestStats(result),
            Some(e),
            getStackDepth("ScalaCheck.scala", "check"),
            FailureMessages("propertyException", UnquotedString(e.getClass.getName)),
            args,
            labels
          )

        case Test.GenException(e) =>
          val (args, labels) = argsAndLabels(result)
          throw new PropertyTestFailedException(
            prettyTestStats(result),
            Some(e),
            getStackDepth("ScalaCheck.scala", "check"),
            FailureMessages("generatorException", UnquotedString(e.getClass.getName)),
            args,
            labels
          )

        case _ =>
          val (args, labels) = argsAndLabels(result)
          throw new PropertyTestFailedException(prettyTestStats(result), None, getStackDepth("ScalaCheck.scala", "check"), "FILL ME IN", args, labels)
      }
    }
  }

  private def argsAndLabels(result: Test.Result): (List[Any], List[String]) = {

    val (scalaCheckArgs, scalaCheckLabels) =
      result.status match {
        case Test.Proved(args) => (args.toList, List())
        case Test.Failed(args, labels) => (args.toList, labels.toList)
        case Test.PropException(args, _, labels) => (args.toList, labels.toList)
        case _ => (List(), List())
      }

    val args: List[Any] = for (scalaCheckArg <- scalaCheckArgs.toList) yield scalaCheckArg.arg

    // scalaCheckLabels is a Set[String], I think
    val labels: List[String] = for (scalaCheckLabel <- scalaCheckLabels.elements.toList) yield scalaCheckLabel

    (args, labels)
  }

  /**
   * Check a property.
   *
   * @param p the property to check
   * @throws TestFailedException if a test case is discovered for which the property doesn't hold.
   */
  def check(p: Prop) {
    check(p, Test.defaultParams)
  }

  // TODO: Internationalize these, and make them consistent with FailureMessages stuff (only strings get quotes around them, etc.)
  private def prettyTestStats(result: Test.Result) = result.status match {

    case Test.Proved(args) =>
      "OK, proved property:                   \n" + prettyArgs(args)

    case Test.Passed =>
      "OK, passed " + result.succeeded + " tests."

    case Test.Failed(args, labels) =>
      "Falsified after " + result.succeeded + " passed tests:\n" + prettyLabels(labels) + prettyArgs(args)

    case Test.Exhausted =>
      "Gave up after only " + result.succeeded + " passed tests. " +
          result.discarded + " tests were discarded."

    case Test.PropException(args, e, labels) =>
      "Exception \"" + e + "\" (included as the TestFailedException's cause) was thrown during property evaluation:\n" + prettyLabels(labels) + prettyArgs(args)

    case Test.GenException(e) =>
      "Exception \"" + e + "\" (included as the TestFailedException's cause) was thrown during argument generation."
  }

  private def prettyLabels(labels: Set[String]) = {
    if (labels.isEmpty) ""
    else if (labels.size == 1) "Label of failing property: " + labels.elements.next + "\n"
    else "Labels of failing property: " + labels.mkString("\n") + "\n"
  }

  private def prettyArgs(args: List[Arg[_]]) = {
    val strs = for((a,i) <- args.zipWithIndex) yield (
      "> " +
      (if (a.label == "") "ARG_" + i else a.label) +
      " = \"" + a.arg +
      (if (a.shrinks > 0) "\" (" + a.shrinks + " shrinks)" else "\"")
    )
    strs.mkString("\n")
  }
}

