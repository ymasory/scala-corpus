/*
 * Copyright 2001-2009 Artima, Inc.
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

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.events._

class FlatSpecSpec extends Spec with SharedHelpers with GivenWhenThen {

  describe("A FlatSpec") {

    it("should return the test names in registration order from testNames when using 'it should'") {

      val a = new FlatSpec {
        it should "test this" in {}
        it should "test that" in {}
      }

      expect(List("should test this", "should test that")) {
        a.testNames.elements.toList
      }

      val b = new FlatSpec {}

      expect(List[String]()) {
        b.testNames.elements.toList
      }

      val c = new FlatSpec {
        it should "test that" in {}
        it should "test this" in {}
      }

      expect(List("should test that", "should test this")) {
        c.testNames.elements.toList
      }

      val d = new FlatSpec {
        behavior of "A Tester"
        it should "test that" in {}
        it should "test this" in {}
      }

      expect(List("A Tester should test that", "A Tester should test this")) {
        d.testNames.elements.toList
      }

      val e = new FlatSpec {
        behavior of "A Tester"
        it should "test this" in {}
        it should "test that" in {}
      }

      expect(List("A Tester should test this", "A Tester should test that")) {
        e.testNames.elements.toList
      }
    }

    it("should throw DuplicateTestNameException if a duplicate test name registration is attempted") {
      
      intercept[DuplicateTestNameException] {
        new FlatSpec {
          it should "test this" in {}
          it should "test this" in {}
        }
      }
      intercept[DuplicateTestNameException] {
        new FlatSpec {
          it should "test this" in {}
          ignore should "test this" in {}
        }
      }
      intercept[DuplicateTestNameException] {
        new FlatSpec {
          ignore should "test this" in {}
          it should "test this" ignore {}
        }
      }
      intercept[DuplicateTestNameException] {
        new FlatSpec {
          ignore should "test this" in {}
          it should "test this" in {}
        }
      }
    }

    it("should return registered tags, including ignore tags, from the tags method") {

      val a = new FlatSpec {
        ignore should "test this" in {}
        it should "test that" in {}
      }
      expect(Map("should test this" -> Set("org.scalatest.Ignore"))) {
        a.tags
      }

      val b = new FlatSpec {
        it should "test this" in {}
        ignore should "test that" in {}
      }
      expect(Map("should test that" -> Set("org.scalatest.Ignore"))) {
        b.tags
      }

      val c = new FlatSpec {
        ignore should "test this" in {}
        ignore should "test that" in {}
      }
      expect(Map("should test this" -> Set("org.scalatest.Ignore"), "should test that" -> Set("org.scalatest.Ignore"))) {
        c.tags
      }

      val d = new FlatSpec {
        it should "test this" in {}
        it should "test that" in {}
      }
      expect(Map()) {
        d.tags
      }

      val e = new FlatSpec {
        it should "test this" taggedAs(mytags.SlowAsMolasses) in {}
        ignore should "test that" taggedAs(mytags.SlowAsMolasses) in {}
      }
      expect(Map("should test this" -> Set("org.scalatest.SlowAsMolasses"), "should test that" -> Set("org.scalatest.Ignore", "org.scalatest.SlowAsMolasses"))) {
        e.tags
      }

      val f = new FlatSpec {}
      expect(Map()) {
        f.tags
      }

      val g = new FlatSpec {
        it should "test this" taggedAs(mytags.SlowAsMolasses, mytags.WeakAsAKitten) in {}
        it should "test that" taggedAs(mytags.SlowAsMolasses) in {}
      }
      expect(Map("should test this" -> Set("org.scalatest.SlowAsMolasses", "org.scalatest.WeakAsAKitten"), "should test that" -> Set("org.scalatest.SlowAsMolasses"))) {
        g.tags
      }
    }

    it("should invoke withFixture from runTest") {
      val a = new FlatSpec {
        var withFixtureWasInvoked = false
        var testWasInvoked = false
        override def withFixture(test: NoArgTest) {
          withFixtureWasInvoked = true
          super.withFixture(test)
        }
        it should "do something" in {
          testWasInvoked = true
        }
      }
      a.run(None, SilentReporter, new Stopper {}, Filter(), Map(), None, new Tracker())
      assert(a.withFixtureWasInvoked)
      assert(a.testWasInvoked)
    }
    it("should pass the correct test name in the NoArgTest passed to withFixture") {
      val a = new FlatSpec {
        var correctTestNameWasPassed = false
        override def withFixture(test: NoArgTest) {
          correctTestNameWasPassed = test.name == "should do something"
          super.withFixture(test)
        }
        it should "do something" in {}
      }
      a.run(None, SilentReporter, new Stopper {}, Filter(), Map(), None, new Tracker())
      assert(a.correctTestNameWasPassed)
    }
    it("should pass the correct config map in the NoArgTest passed to withFixture") {
      val a = new FlatSpec {
        var correctConfigMapWasPassed = false
        override def withFixture(test: NoArgTest) {
          correctConfigMapWasPassed = (test.configMap == Map("hi" -> 7))
          super.withFixture(test)
        }
        it should "do something" in {}
      }
      a.run(None, SilentReporter, new Stopper {}, Filter(), Map("hi" -> 7), None, new Tracker())
      assert(a.correctConfigMapWasPassed)
    }
    describe("(with info calls)") {
      class InfoInsideTestFlatSpec extends FlatSpec {
        val msg = "hi there, dude"
        val partialTestName = "test name"
        val testName = "should " + partialTestName
        it should partialTestName in {
          info(msg)
        }
      }
      // In a FlatSpec, any InfoProvided's fired during the test should be cached and sent out after the test has
      // suceeded or failed. This makes the report look nicer, because the info is tucked under the "specifier'
      // text for that test.
      it("should, when the info appears in the code of a successful test, report the info after the TestSucceeded") {
        val spec = new InfoInsideTestFlatSpec
        val (infoProvidedIndex, testStartingIndex, testSucceededIndex) =
          getIndexesForInformerEventOrderTests(spec, spec.testName, spec.msg)
        assert(testSucceededIndex < infoProvidedIndex)
      }
      class InfoBeforeTestFlatSpec extends FlatSpec {
        val msg = "hi there, dude"
        val partialTestName = "test name"
        val testName = "should " + partialTestName
        info(msg)
        it should partialTestName in {}
      }
      it("should, when the info appears in the body before a test, report the info before the test") {
        val spec = new InfoBeforeTestFlatSpec
        val (infoProvidedIndex, testStartingIndex, testSucceededIndex) =
          getIndexesForInformerEventOrderTests(spec, spec.testName, spec.msg)
        assert(infoProvidedIndex < testStartingIndex)
        assert(testStartingIndex < testSucceededIndex)
      }
      it("should, when the info appears in the body after a test, report the info after the test runs") {
        val msg = "hi there, dude"
        val partialTestName = "test name"
        val testName = "should " + partialTestName
        class MyFlatSpec extends FlatSpec {
          it should partialTestName in {}
          info(msg)
        }
        val (infoProvidedIndex, testStartingIndex, testSucceededIndex) =
          getIndexesForInformerEventOrderTests(new MyFlatSpec, testName, msg)
        assert(testStartingIndex < testSucceededIndex)
        assert(testSucceededIndex < infoProvidedIndex)
      }
      it("should throw an IllegalStateException when info is called by a method invoked after the suite has been executed") {
        class MyFlatSpec extends FlatSpec {
          callInfo() // This should work fine
          def callInfo() {
            info("howdy")
          }
          it should "howdy also" in {
            callInfo() // This should work fine
          }
        }
        val spec = new MyFlatSpec
        val myRep = new EventRecordingReporter
        spec.run(None, myRep, new Stopper {}, Filter(), Map(), None, new Tracker)
        intercept[IllegalStateException] {
          spec.callInfo()
        }
      }
      it("should send an InfoProvided with an IndentedText formatter with level 1 when called outside a test") {
        val spec = new InfoBeforeTestFlatSpec
        val indentedText = getIndentedTextFromInfoProvided(spec)
        assert(indentedText === IndentedText("+ " + spec.msg, spec.msg, 1))
      }
      it("should send an InfoProvided with an IndentedText formatter with level 2 when called within a test") {
        val spec = new InfoInsideTestFlatSpec
        val indentedText = getIndentedTextFromInfoProvided(spec)
        assert(indentedText === IndentedText("  + " + spec.msg, spec.msg, 2))
      }
      it("should work when using the shorthand notation for 'behavior of'") {
        val e = new FlatSpec with ShouldMatchers {
          "A Tester" should "test this" in {}
          it should "test that" in {}
        }

        expect(List("A Tester should test this", "A Tester should test that")) {
          e.testNames.elements.toList
        }

      }
    }
    describe("(when a nesting rule has been violated)") {

      it("should, if they call a behavior-of from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FlatSpec {
          it should "blow up" in {
            behavior of "in the wrong place, at the wrong time"
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a behavior-of with a nested it from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FlatSpec {
          it should "blow up" in {
            behavior of "in the wrong place, at the wrong time"
            it should "never run" in {
              assert(1 === 1)
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested it from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FlatSpec {
          it should "blow up" in {
            it should "never run" in {
              assert(1 === 1)
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested it with tags from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FlatSpec {
          it should "blow up" in {
            it should "never run" taggedAs(mytags.SlowAsMolasses) in {
              assert(1 === 1)
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a behavior-of with a nested ignore from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FlatSpec {
          it should "blow up" in {
            behavior of "in the wrong place, at the wrong time"
            ignore should "never run" in {
              assert(1 === 1)
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested ignore from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FlatSpec {
          it should "blow up" in {
            ignore should "never run" in {
              assert(1 === 1)
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested ignore with tags from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FlatSpec {
          it should "blow up" in {
            ignore should "never run" taggedAs(mytags.SlowAsMolasses) in {
              assert(1 === 1)
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
    }
    it("should run tests registered via the 'it should behave like' syntax") {
      trait SharedFlatSpecTests { this: FlatSpec =>
        def nonEmptyStack(s: String)(i: Int) {
          it should "I am shared" in {}
        }
      }
      class MyFlatSpec extends FlatSpec with SharedFlatSpecTests {
        it should behave like nonEmptyStack("hi")(1)
      }
      val suite = new MyFlatSpec
      val reporter = new EventRecordingReporter
      suite.run(None, reporter, new Stopper {}, Filter(), Map(), None, new Tracker)

      val indexedList = reporter.eventsReceived

      val testStartingOption = indexedList.find(_.isInstanceOf[TestStarting])
      assert(testStartingOption.isDefined)
      assert(testStartingOption.get.asInstanceOf[TestStarting].testName === "should I am shared")
    }
    it("should run tests registered via the 'it can behave like' syntax") {
      trait SharedFlatSpecTests { this: FlatSpec =>
        def nonEmptyStack(s: String)(i: Int) {
          it can "I am shared" in {}
        }
      }
      class MyFlatSpec extends FlatSpec with SharedFlatSpecTests {
        it can behave like nonEmptyStack("hi")(1)
      }
      val suite = new MyFlatSpec
      val reporter = new EventRecordingReporter
      suite.run(None, reporter, new Stopper {}, Filter(), Map(), None, new Tracker)

      val indexedList = reporter.eventsReceived

      val testStartingOption = indexedList.find(_.isInstanceOf[TestStarting])
      assert(testStartingOption.isDefined)
      assert(testStartingOption.get.asInstanceOf[TestStarting].testName === "can I am shared")
    }
    it("should throw NullPointerException if a null test tag is provided") {
      // it
      intercept[NullPointerException] {
        new FlatSpec {
          it should "hi" taggedAs(null) in {}
        }
      }
      val caught = intercept[NullPointerException] {
        new FlatSpec {
          it should "hi" taggedAs(mytags.SlowAsMolasses, null) in {}
        }
      }
      assert(caught.getMessage === "a test tag was null")
      intercept[NullPointerException] {
        new FlatSpec {
          it should "hi" taggedAs(mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) in {}
        }
      }
      // ignore
      intercept[NullPointerException] {
        new FlatSpec {
          ignore should "hi" taggedAs(null) in {}
        }
      }
      val caught2 = intercept[NullPointerException] {
        new FlatSpec {
          ignore should "hi" taggedAs(mytags.SlowAsMolasses, null) in {}
        }
      }
      assert(caught2.getMessage === "a test tag was null")
      intercept[NullPointerException] {
        new FlatSpec {
          ignore should "hi" taggedAs(mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) in {}
        }
      }
      intercept[NullPointerException] {
        new FlatSpec {
          it should "hi" taggedAs(null) ignore {}
        }
      }
      val caught3 = intercept[NullPointerException] {
        new FlatSpec {
          it should "hi" taggedAs(mytags.SlowAsMolasses, null) ignore {}
        }
      }
      assert(caught3.getMessage === "a test tag was null")
      intercept[NullPointerException] {
        new FlatSpec {
          it should "hi" taggedAs(mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) ignore {}
        }
      }
    }
    it("should return a correct tags map from the tags method, when using regular (not shorthand)" +
            " notation and ignore replacing it") {

      val a = new FlatSpec {
        ignore should "test this" in {}
        it should "test that" in {}
      }
      expect(Map("should test this" -> Set("org.scalatest.Ignore"))) {
        a.tags
      }

      val b = new FlatSpec {
        it can "test this" in {}
        ignore can "test that" in {}
      }
      expect(Map("can test that" -> Set("org.scalatest.Ignore"))) {
        b.tags
      }

      val c = new FlatSpec {
        ignore must "test this" in {}
        ignore must "test that" in {}
      }
      expect(Map("must test this" -> Set("org.scalatest.Ignore"), "must test that" -> Set("org.scalatest.Ignore"))) {
        c.tags
      }

      val d = new FlatSpec {
        it must "test this" taggedAs(mytags.SlowAsMolasses) in {}
        ignore must "test that" taggedAs(mytags.SlowAsMolasses) in {}
      }
      expect(Map("must test this" -> Set("org.scalatest.SlowAsMolasses"), "must test that" -> Set("org.scalatest.Ignore", "org.scalatest.SlowAsMolasses"))) {
        d.tags
      }

      val e = new FlatSpec {
        it must "test this" in {}
        it must "test that" in {}
      }
      expect(Map()) {
        e.tags
      }

      val f = new FlatSpec {
        it can "test this" taggedAs(mytags.SlowAsMolasses, mytags.WeakAsAKitten) in {}
        it can "test that" taggedAs(mytags.SlowAsMolasses) in  {}
      }
      expect(Map("can test this" -> Set("org.scalatest.SlowAsMolasses", "org.scalatest.WeakAsAKitten"), "can test that" -> Set("org.scalatest.SlowAsMolasses"))) {
        f.tags
      }

      val g = new FlatSpec {
        it should "test this" taggedAs(mytags.SlowAsMolasses, mytags.WeakAsAKitten) in {}
        it should "test that" taggedAs(mytags.SlowAsMolasses) in  {}
      }
      expect(Map("should test this" -> Set("org.scalatest.SlowAsMolasses", "org.scalatest.WeakAsAKitten"), "should test that" -> Set("org.scalatest.SlowAsMolasses"))) {
        g.tags
      }
    }
    it("should return a correct tags map from the tags method, when using regular (not shorthand)" +
            " notation and ignore replacing in") {

      val a = new FlatSpec {
        it should "test this" ignore {}
        it should "test that" in {}
      }
      expect(Map("should test this" -> Set("org.scalatest.Ignore"))) {
        a.tags
      }

      val b = new FlatSpec {
        it can "test this" in {}
        it can "test that" ignore {}
      }
      expect(Map("can test that" -> Set("org.scalatest.Ignore"))) {
        b.tags
      }

      val c = new FlatSpec {
        it must "test this" ignore {}
        it must "test that" ignore {}
      }
      expect(Map("must test this" -> Set("org.scalatest.Ignore"), "must test that" -> Set("org.scalatest.Ignore"))) {
        c.tags
      }

      val d = new FlatSpec {
        it must "test this" taggedAs(mytags.SlowAsMolasses) in {}
        it must "test that" taggedAs(mytags.SlowAsMolasses) ignore {}
      }
      expect(Map("must test this" -> Set("org.scalatest.SlowAsMolasses"), "must test that" -> Set("org.scalatest.Ignore", "org.scalatest.SlowAsMolasses"))) {
        d.tags
      }
    }

    it("should return a correct tags map from the tags method, when using shorthand notation") {

      val a = new FlatSpec {
        "A Stack" should "test this" ignore {}
        "A Stack" should "test that" in {}
      }
      expect(Map("A Stack should test this" -> Set("org.scalatest.Ignore"))) {
        a.tags
      }

      val b = new FlatSpec {
        "A Stack" can "test this" in {}
        "A Stack" can "test that" ignore {}
      }
      expect(Map("A Stack can test that" -> Set("org.scalatest.Ignore"))) {
        b.tags
      }

      val c = new FlatSpec {
        "A Stack" must "test this" ignore {}
        "A Stack" must "test that" ignore {}
      }
      expect(Map("A Stack must test this" -> Set("org.scalatest.Ignore"), "A Stack must test that" -> Set("org.scalatest.Ignore"))) {
        c.tags
      }

      val d = new FlatSpec {
        "A Stack" must "test this" taggedAs(mytags.SlowAsMolasses) in {}
        "A Stack" must "test that" taggedAs(mytags.SlowAsMolasses) ignore {}
      }
      expect(Map("A Stack must test this" -> Set("org.scalatest.SlowAsMolasses"), "A Stack must test that" -> Set("org.scalatest.Ignore", "org.scalatest.SlowAsMolasses"))) {
        d.tags
      }

      val e = new FlatSpec {
        "A Stack" must "test this" in {}
        "A Stack" must "test that" in {}
      }
      expect(Map()) {
        e.tags
      }

      val f = new FlatSpec {
        "A Stack" can "test this" taggedAs(mytags.SlowAsMolasses, mytags.WeakAsAKitten) in {}
        "A Stack" can "test that" taggedAs(mytags.SlowAsMolasses) in  {}
      }
      expect(Map("A Stack can test this" -> Set("org.scalatest.SlowAsMolasses", "org.scalatest.WeakAsAKitten"), "A Stack can test that" -> Set("org.scalatest.SlowAsMolasses"))) {
        f.tags
      }

      val g = new FlatSpec {
        "A Stack" should "test this" taggedAs(mytags.SlowAsMolasses, mytags.WeakAsAKitten) in {}
        "A Stack" should "test that" taggedAs(mytags.SlowAsMolasses) in  {}
      }
      expect(Map("A Stack should test this" -> Set("org.scalatest.SlowAsMolasses", "org.scalatest.WeakAsAKitten"), "A Stack should test that" -> Set("org.scalatest.SlowAsMolasses"))) {
        g.tags
      }
    }

    it("should return a correct tags map from the tags method when is (pending), when using regular (not shorthand)" +
            " notation and ignore replacing it") {

      val a = new FlatSpec {
        ignore should "test this" is (pending)
        it should "test that" is (pending)
      }
      expect(Map("should test this" -> Set("org.scalatest.Ignore"))) {
        a.tags
      }

      val b = new FlatSpec {
        it can "test this" is (pending)
        ignore can "test that" is (pending)
      }
      expect(Map("can test that" -> Set("org.scalatest.Ignore"))) {
        b.tags
      }

      val c = new FlatSpec {
        ignore must "test this" is (pending)
        ignore must "test that" is (pending)
      }
      expect(Map("must test this" -> Set("org.scalatest.Ignore"), "must test that" -> Set("org.scalatest.Ignore"))) {
        c.tags
      }

      val d = new FlatSpec {
        it must "test this" taggedAs(mytags.SlowAsMolasses) is (pending)
        ignore must "test that" taggedAs(mytags.SlowAsMolasses) is (pending)
      }
      expect(Map("must test this" -> Set("org.scalatest.SlowAsMolasses"), "must test that" -> Set("org.scalatest.Ignore", "org.scalatest.SlowAsMolasses"))) {
        d.tags
      }

      val e = new FlatSpec {
        it must "test this" is (pending)
        it must "test that" is (pending)
      }
      expect(Map()) {
        e.tags
      }

      val f = new FlatSpec {
        it can "test this" taggedAs(mytags.SlowAsMolasses, mytags.WeakAsAKitten) is (pending)
        it can "test that" taggedAs(mytags.SlowAsMolasses) in  {}
      }
      expect(Map("can test this" -> Set("org.scalatest.SlowAsMolasses", "org.scalatest.WeakAsAKitten"), "can test that" -> Set("org.scalatest.SlowAsMolasses"))) {
        f.tags
      }

      val g = new FlatSpec {
        it should "test this" taggedAs(mytags.SlowAsMolasses, mytags.WeakAsAKitten) is (pending)
        it should "test that" taggedAs(mytags.SlowAsMolasses) in  {}
      }
      expect(Map("should test this" -> Set("org.scalatest.SlowAsMolasses", "org.scalatest.WeakAsAKitten"), "should test that" -> Set("org.scalatest.SlowAsMolasses"))) {
        g.tags
      }
    }
    it("should return a correct tags map from the tags method is (pending), when using regular (not shorthand)" +
            " notation and ignore replacing is") {

      val a = new FlatSpec {
        it should "test this" ignore {}
        it should "test that" is (pending)
      }
      expect(Map("should test this" -> Set("org.scalatest.Ignore"))) {
        a.tags
      }

      val b = new FlatSpec {
        it can "test this" is (pending)
        it can "test that" ignore {}
      }
      expect(Map("can test that" -> Set("org.scalatest.Ignore"))) {
        b.tags
      }

      val c = new FlatSpec {
        it must "test this" ignore {}
        it must "test that" ignore {}
      }
      expect(Map("must test this" -> Set("org.scalatest.Ignore"), "must test that" -> Set("org.scalatest.Ignore"))) {
        c.tags
      }

      val d = new FlatSpec {
        it must "test this" taggedAs(mytags.SlowAsMolasses) is (pending)
        it must "test that" taggedAs(mytags.SlowAsMolasses) ignore {}
      }
      expect(Map("must test this" -> Set("org.scalatest.SlowAsMolasses"), "must test that" -> Set("org.scalatest.Ignore", "org.scalatest.SlowAsMolasses"))) {
        d.tags
      }
    }

    it("should return a correct tags map from the tags method is (pending), when using shorthand notation") {

      val a = new FlatSpec {
        "A Stack" should "test this" ignore {}
        "A Stack" should "test that" is (pending)
      }
      expect(Map("A Stack should test this" -> Set("org.scalatest.Ignore"))) {
        a.tags
      }

      val b = new FlatSpec {
        "A Stack" can "test this" is (pending)
        "A Stack" can "test that" ignore {}
      }
      expect(Map("A Stack can test that" -> Set("org.scalatest.Ignore"))) {
        b.tags
      }

      val c = new FlatSpec {
        "A Stack" must "test this" ignore {}
        "A Stack" must "test that" ignore {}
      }
      expect(Map("A Stack must test this" -> Set("org.scalatest.Ignore"), "A Stack must test that" -> Set("org.scalatest.Ignore"))) {
        c.tags
      }

      val d = new FlatSpec {
        "A Stack" must "test this" taggedAs(mytags.SlowAsMolasses) is (pending)
        "A Stack" must "test that" taggedAs(mytags.SlowAsMolasses) ignore {}
      }
      expect(Map("A Stack must test this" -> Set("org.scalatest.SlowAsMolasses"), "A Stack must test that" -> Set("org.scalatest.Ignore", "org.scalatest.SlowAsMolasses"))) {
        d.tags
      }

      val e = new FlatSpec {
        "A Stack" must "test this" is (pending)
        "A Stack" must "test that" is (pending)
      }
      expect(Map()) {
        e.tags
      }

      val f = new FlatSpec {
        "A Stack" can "test this" taggedAs(mytags.SlowAsMolasses, mytags.WeakAsAKitten) is (pending)
        "A Stack" can "test that" taggedAs(mytags.SlowAsMolasses) is (pending)
      }
      expect(Map("A Stack can test this" -> Set("org.scalatest.SlowAsMolasses", "org.scalatest.WeakAsAKitten"), "A Stack can test that" -> Set("org.scalatest.SlowAsMolasses"))) {
        f.tags
      }

      val g = new FlatSpec {
        "A Stack" should "test this" taggedAs(mytags.SlowAsMolasses, mytags.WeakAsAKitten) is (pending)
        "A Stack" should "test that" taggedAs(mytags.SlowAsMolasses) is (pending)
      }
      expect(Map("A Stack should test this" -> Set("org.scalatest.SlowAsMolasses", "org.scalatest.WeakAsAKitten"), "A Stack should test that" -> Set("org.scalatest.SlowAsMolasses"))) {
        g.tags
      }
    }

    class TestWasCalledSuite extends FlatSpec {
      var theTestThisCalled = false
      var theTestThatCalled = false
      it should "run this" in { theTestThisCalled = true }
      it should "run that, maybe" in { theTestThatCalled = true }
    }

    it("should execute all tests when run is called with testName None") {

      val b = new TestWasCalledSuite
      b.run(None, SilentReporter, new Stopper {}, Filter(), Map(), None, new Tracker)
      assert(b.theTestThisCalled)
      assert(b.theTestThatCalled)
    }

    it("should execute one test when run is called with a defined testName") {

      val a = new TestWasCalledSuite
      a.run(Some("should run this"), SilentReporter, new Stopper {}, Filter(), Map(), None, new Tracker)
      assert(a.theTestThisCalled)
      assert(!a.theTestThatCalled)
    }

    it("should report as ignored, and not run, tests marked ignored") {

      val a = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        it can "test this" in { theTestThisCalled = true }
        it can "test that" in { theTestThatCalled = true }
      }

      val repA = new TestIgnoredTrackingReporter
      a.run(None, repA, new Stopper {}, Filter(), Map(), None, new Tracker)
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      val b = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore must "test this" in { theTestThisCalled = true }
        it must "test that" in { theTestThatCalled = true }
      }

      val repB = new TestIgnoredTrackingReporter
      b.run(None, repB, new Stopper {}, Filter(), Map(), None, new Tracker)
      assert(repB.testIgnoredReceived)
      assert(repB.lastEvent.isDefined)
      assert(repB.lastEvent.get.testName endsWith "test this")
      assert(!b.theTestThisCalled)
      assert(b.theTestThatCalled)

      val c = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        it can "test this" in { theTestThisCalled = true }
        ignore can "test that" in { theTestThatCalled = true }
      }

      val repC = new TestIgnoredTrackingReporter
      c.run(None, repC, new Stopper {}, Filter(), Map(), None, new Tracker)
      assert(repC.testIgnoredReceived)
      assert(repC.lastEvent.isDefined)
      assert(repC.lastEvent.get.testName endsWith "test that", repC.lastEvent.get.testName)
      assert(c.theTestThisCalled)
      assert(!c.theTestThatCalled)

      // The order I want is order of appearance in the file.
      // Will try and implement that tomorrow. Subtypes will be able to change the order.
      val d = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore should "test this" in { theTestThisCalled = true }
        ignore should "test that" in { theTestThatCalled = true }
      }

      val repD = new TestIgnoredTrackingReporter
      d.run(None, repD, new Stopper {}, Filter(), Map(), None, new Tracker)
      assert(repD.testIgnoredReceived)
      assert(repD.lastEvent.isDefined)
      assert(repD.lastEvent.get.testName endsWith "test that") // last because should be in order of appearance
      assert(!d.theTestThisCalled)
      assert(!d.theTestThatCalled)
    }

    it("should run a test marked as ignored if run is invoked with that testName") {
      // If I provide a specific testName to run, then it should ignore an Ignore on that test
      // method and actually invoke it.
      val e = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore must "test this" in { theTestThisCalled = true }
        it must "test that" in { theTestThatCalled = true }
      }

      val repE = new TestIgnoredTrackingReporter
      e.run(Some("must test this"), repE, new Stopper {}, Filter(), Map(), None, new Tracker)
      assert(!repE.testIgnoredReceived)
      assert(e.theTestThisCalled)
      assert(!e.theTestThatCalled)
    }

    it("should run only those tests selected by the tags to include and exclude sets") {

      // Nothing is excluded
      val a = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        it should "test this" taggedAs(mytags.SlowAsMolasses) in { theTestThisCalled = true }
        it should "test that" in { theTestThatCalled = true }
      }
      val repA = new TestIgnoredTrackingReporter
      a.run(None, repA, new Stopper {}, Filter(), Map(), None, new Tracker)
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      // SlowAsMolasses is included, one test should be excluded
      val b = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        it should "test this" taggedAs(mytags.SlowAsMolasses) in { theTestThisCalled = true }
        it should "test that" in { theTestThatCalled = true }
      }
      val repB = new TestIgnoredTrackingReporter
      b.run(None, repB, new Stopper {}, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), Map(), None, new Tracker)
      assert(!repB.testIgnoredReceived)
      assert(b.theTestThisCalled)
      assert(!b.theTestThatCalled)

      // SlowAsMolasses is included, and both tests should be included
      val c = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        it should "test this" taggedAs(mytags.SlowAsMolasses) in { theTestThisCalled = true }
        it should "test that" taggedAs(mytags.SlowAsMolasses) in { theTestThatCalled = true }
      }
      val repC = new TestIgnoredTrackingReporter
      c.run(None, repB, new Stopper {}, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), Map(), None, new Tracker)
      assert(!repC.testIgnoredReceived)
      assert(c.theTestThisCalled)
      assert(c.theTestThatCalled)

      // SlowAsMolasses is included. both tests should be included but one ignored
      val d = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore should "test this" taggedAs(mytags.SlowAsMolasses) in { theTestThisCalled = true }
        it should "test that" taggedAs(mytags.SlowAsMolasses) in { theTestThatCalled = true }
      }
      val repD = new TestIgnoredTrackingReporter
      d.run(None, repD, new Stopper {}, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.Ignore")), Map(), None, new Tracker)
      assert(repD.testIgnoredReceived)
      assert(!d.theTestThisCalled)
      assert(d.theTestThatCalled)

      // SlowAsMolasses included, FastAsLight excluded
      val e = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        it should "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) in { theTestThisCalled = true }
        it should "test that" taggedAs(mytags.SlowAsMolasses) in { theTestThatCalled = true }
        it should "test the other" in { theTestTheOtherCalled = true }
      }
      val repE = new TestIgnoredTrackingReporter
      e.run(None, repE, new Stopper {}, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                Map(), None, new Tracker)
      assert(!repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(e.theTestThatCalled)
      assert(!e.theTestTheOtherCalled)

      // An Ignored test that was both included and excluded should not generate a TestIgnored event
      val f = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        ignore should "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) in { theTestThisCalled = true }
        it should "test that" taggedAs(mytags.SlowAsMolasses) in { theTestThatCalled = true }
        it should "test the other" in { theTestTheOtherCalled = true }
      }
      val repF = new TestIgnoredTrackingReporter
      f.run(None, repF, new Stopper {}, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                Map(), None, new Tracker)
      assert(!repF.testIgnoredReceived)
      assert(!f.theTestThisCalled)
      assert(f.theTestThatCalled)
      assert(!f.theTestTheOtherCalled)

      // An Ignored test that was not included should not generate a TestIgnored event
      val g = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        it should "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) in { theTestThisCalled = true }
        it should "test that" taggedAs(mytags.SlowAsMolasses) in { theTestThatCalled = true }
        ignore should "test the other" in { theTestTheOtherCalled = true }
      }
      val repG = new TestIgnoredTrackingReporter
      g.run(None, repG, new Stopper {}, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                Map(), None, new Tracker)
      assert(!repG.testIgnoredReceived)
      assert(!g.theTestThisCalled)
      assert(g.theTestThatCalled)
      assert(!g.theTestTheOtherCalled)

      // No tagsToInclude set, FastAsLight excluded
      val h = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        it should "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) in { theTestThisCalled = true }
        it should "test that" taggedAs(mytags.SlowAsMolasses) in { theTestThatCalled = true }
        it should "test the other" in { theTestTheOtherCalled = true }
      }
      val repH = new TestIgnoredTrackingReporter
      h.run(None, repH, new Stopper {}, Filter(None, Set("org.scalatest.FastAsLight")), Map(), None, new Tracker)
      assert(!repH.testIgnoredReceived)
      assert(!h.theTestThisCalled)
      assert(h.theTestThatCalled)
      assert(h.theTestTheOtherCalled)

      // No tagsToInclude set, mytags.SlowAsMolasses excluded
      val i = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        it should "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) in { theTestThisCalled = true }
        it should "test that" taggedAs(mytags.SlowAsMolasses) in { theTestThatCalled = true }
        it should "test the other" in { theTestTheOtherCalled = true }
      }
      val repI = new TestIgnoredTrackingReporter
      i.run(None, repI, new Stopper {}, Filter(None, Set("org.scalatest.SlowAsMolasses")), Map(), None, new Tracker)
      assert(!repI.testIgnoredReceived)
      assert(!i.theTestThisCalled)
      assert(!i.theTestThatCalled)
      assert(i.theTestTheOtherCalled)

      // No tagsToInclude set, mytags.SlowAsMolasses excluded, TestIgnored should not be received on excluded ones
      val j = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        ignore should "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) in { theTestThisCalled = true }
        ignore should "test that" taggedAs(mytags.SlowAsMolasses) in { theTestThatCalled = true }
        it should "test the other" in { theTestTheOtherCalled = true }
      }
      val repJ = new TestIgnoredTrackingReporter
      j.run(None, repJ, new Stopper {}, Filter(None, Set("org.scalatest.SlowAsMolasses")), Map(), None, new Tracker)
      assert(!repI.testIgnoredReceived)
      assert(!j.theTestThisCalled)
      assert(!j.theTestThatCalled)
      assert(j.theTestTheOtherCalled)

      // Same as previous, except Ignore specifically mentioned in excludes set
      val k = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        ignore should "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) in { theTestThisCalled = true }
        ignore should "test that" taggedAs(mytags.SlowAsMolasses) in { theTestThatCalled = true }
        ignore should "test the other" in { theTestTheOtherCalled = true }
      }
      val repK = new TestIgnoredTrackingReporter
      k.run(None, repK, new Stopper {}, Filter(None, Set("org.scalatest.SlowAsMolasses", "org.scalatest.Ignore")), Map(), None, new Tracker)
      assert(repK.testIgnoredReceived)
      assert(!k.theTestThisCalled)
      assert(!k.theTestThatCalled)
      assert(!k.theTestTheOtherCalled)
    }

    it("should return the correct test count from its expectedTestCount method") {

      val a = new FlatSpec {
        it should "test this" in {}
        it should "test that" in {}
      }
      assert(a.expectedTestCount(Filter()) === 2)

      val b = new FlatSpec {
        ignore should "test this" in {}
        it should "test that" in {}
      }
      assert(b.expectedTestCount(Filter()) === 1)

      val c = new FlatSpec {
        it should "test this" taggedAs(mytags.FastAsLight) in {}
        it should "test that" in {}
      }
      assert(c.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(c.expectedTestCount(Filter(None, Set("org.scalatest.FastAsLight"))) === 1)

      val d = new FlatSpec {
        it should "test this" taggedAs(mytags.FastAsLight, mytags.SlowAsMolasses) in {}
        it should "test that" taggedAs(mytags.SlowAsMolasses) in {}
        it should "test the other thing" in {}
      }
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 1)
      assert(d.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 1)
      assert(d.expectedTestCount(Filter()) === 3)

      val e = new FlatSpec {
        it should "test this" taggedAs(mytags.FastAsLight, mytags.SlowAsMolasses) in {}
        it should "test that" taggedAs(mytags.SlowAsMolasses) in {}
        ignore should "test the other thing" in {}
      }
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 1)
      assert(e.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 0)
      assert(e.expectedTestCount(Filter()) === 2)

      val f = new SuperSuite(List(a, b, c, d, e))
      assert(f.expectedTestCount(Filter()) === 10)
    }

    it("should generate a TestPending message when the test body is (pending)") {

      val a = new FlatSpec {

        it should "do this" is (pending)

        it should "do that" in {
          assert(2 + 2 === 4)
        }

        it should "do something else" in {
          assert(2 + 2 === 4)
          pending
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, rep, new Stopper {}, Filter(), Map(), None, new Tracker())
      val tp = rep.testPendingEventsReceived
      assert(tp.size === 2)
    }
    it("should generate a test failure if a Throwable, or an Error other than direct Error subtypes " +
            "known in JDK 1.5, excluding AssertionError") {
      val a = new FlatSpec {
        it should "throw AssertionError" in { throw new AssertionError }
        it should "throw plain old Error" in { throw new Error }
        it should "throw Throwable" in { throw new Throwable }
      }
      val rep = new EventRecordingReporter
      a.run(None, rep, new Stopper {}, Filter(), Map(), None, new Tracker())
      val tf = rep.testFailedEventsReceived
      assert(tf.size === 3)
    }
    it("should propagate out Errors that are direct subtypes of Error in JDK 1.5, other than " +
            "AssertionError, causing Suites and Runs to abort.") {
      val a = new FlatSpec {
        it should "throws AssertionError" in { throw new OutOfMemoryError }
      }
      intercept[OutOfMemoryError] {
        a.run(None, SilentReporter, new Stopper {}, Filter(), Map(), None, new Tracker())
      }
    }
    it("should send InfoProvided events with aboutAPendingTest set to true for info " +
            "calls made from a test that is pending") {
      val a = new FlatSpec with GivenWhenThen {
        it should "do something else" in {
          given("two integers")
          when("one is subracted from the other")
          then("the result is the difference between the two numbers")
          pending
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, rep, new Stopper {}, Filter(), Map(), None, new Tracker())
      val ip = rep.infoProvidedEventsReceived
      assert(ip.size === 3)
      for (event <- ip) {
        assert(event.aboutAPendingTest.isDefined && event.aboutAPendingTest.get)
      }
    }
    it("should send InfoProvided events with aboutAPendingTest set to false for info " +
            "calls made from a test that is not pending") {
      val a = new FlatSpec with GivenWhenThen {
        it should "do something else" in {
          given("two integers")
          when("one is subracted from the other")
          then("the result is the difference between the two numbers")
          assert(1 + 1 === 2)
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, rep, new Stopper {}, Filter(), Map(), None, new Tracker())
      val ip = rep.infoProvidedEventsReceived
      assert(ip.size === 3)
      for (event <- ip) {
        assert(event.aboutAPendingTest.isDefined && !event.aboutAPendingTest.get)
      }
    }
  }
}

