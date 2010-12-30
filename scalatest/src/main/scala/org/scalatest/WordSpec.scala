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

import verb.{CanVerb, ResultOfAfterWordApplication, ShouldVerb, BehaveWord,
  MustVerb, StringVerbBlockRegistration}
import NodeFamily._
import scala.collection.immutable.ListSet
import org.scalatest.StackDepthExceptionHelper.getStackDepth
import java.util.concurrent.atomic.AtomicReference
import java.util.ConcurrentModificationException
import org.scalatest.events._
import Suite.anErrorThatShouldCauseAnAbort

/**
 * Trait that facilitates a &#8220;behavior-driven&#8221; style of development (BDD), in which tests
 * are combined with text that specifies the behavior the tests verify.
 * (In BDD, the word <em>example</em> is usually used instead of <em>test</em>. The word test will not appear
 * in your code if you use <code>WordSpec</code>, so if you prefer the word <em>example</em> you can use it. However, in this documentation
 * the word <em>test</em> will be used, for clarity and to be consistent with the rest of ScalaTest.)
 * Trait <code>WordSpec</code> is so named because
 * you specification text is structured by placing words after strings.
 * Here's an example <code>WordSpec</code>:
 *
 * <pre>
 * import org.scalatest.WordSpec
 * import scala.collection.mutable.Stack
 *
 * class StackSpec extends WordSpec {
 *
 *   "A Stack" should {
 *
 *     "pop values in last-in-first-out order" in {
 *       val stack = new Stack[Int]
 *       stack.push(1)
 *       stack.push(2)
 *       assert(stack.pop() === 2)
 *       assert(stack.pop() === 1)
 *     }
 *
 *     "throw NoSuchElementException if an empty stack is popped" in {
 *       val emptyStack = new Stack[String]
 *       intercept[NoSuchElementException] {
 *         emptyStack.pop()
 *       }
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * <em>Note: Trait <code>WordSpec</code> is in part inspired by class <code>org.specs.Specification</code>, designed by
 * Eric Torreborre for the <a href="http://code.google.com/p/specs/">Specs framework</a>.</em>
 * </p>
 *
 * <p>
 * In a <code>WordSpec</code> you write a one (or more) sentence specification for each bit of behavior you wish to
 * specify and test. Each specification sentence has a
 * "subject," which is sometimes called the <em>system under test</em> (or SUT). The 
 * subject is entity being specified and tested and also serves as the subject of the sentences you write for each test. A subject
 * can be followed by one of three verbs, <code>should</code>, <code>must</code>, or <code>can</code>, and a block. Here are some
 * examples:
 * </p>
 * 
 * <pre>
 * "A Stack" should {
 *   // ...
 * }
 * "An Account" must {
 *   // ...
 * }
 * "A ShippingManifest" can {
 *   // ...
 * }
 * </pre>
 * 
 * <p>
 * You can describe a subject in varying situations by using a <code>when</code> clause. A <code>when</code> clause
 * follows the subject and precedes a block. In the block after the <code>when</code>, you place strings that describe a situation or a state
 * the subject may be in using a string, each followed by a verb. Here's an example:
 * </p>
 *
 * <pre>
 * "A Stack" when {
 *   "empty" should {
 *     // ...
 *   }
 *   "non-empty" should {
 *     // ...
 *   }
 *   "full" should {
 *     // ...
 *   }
 * }
 * </pre>
 * 
 * <p>
 * When you are ready to finish a sentence, you write a string followed by <code>in</code> and a block that
 * contains the code of the test. Here's an example:
 * </p>
 *
 * <pre>
 * import org.scalatest.WordSpec
 * 
 * class StackSpec extends WordSpec {
 *   "A Stack" when {
 *     "empty" should {
 *       "be empty" in {
 *         // ...
 *       }
 *       "complain on peek" in {
 *         // ...
 *       }
 *       "complain on pop" in {
 *         // ...
 *       }
 *     }
 *     "full" should {
 *       "be full" in {
 *         // ...
 *       }
 *       "complain on push" in {
 *         // ...
 *       }
 *     }
 *   }
 * }
 * </pre>
 * 
 * <p>
 * Running the above <code>StackSpec</code> in the interpreter would yield:
 * </p>
 * 
 * <pre>
 * scala> (new StackSpec).execute()
 * A Stack (when empty) 
 * - should be empty 
 * - should complain on peek
 * - should complain on pop
 * A Stack (when full) 
 * - should be full
 * - should complain on push
 * </pre>
 *
 * <p>
 * Note that the output does not exactly match the input in an effort to maximize readability.
 * Although the <code>WordSpec</code> code is nested, which can help you eliminate any repeated phrases
 * in the specification portion of your code, the output printed will have one line per subject per situation, and
 * one line per test.
 * </p>
 *
 * <p>
 * Sometimes you may wish to eliminate repeated phrases inside the block following a <code>verb</code>. Here's an example
 * in which the phrase "provide an and/or operator that" is repeated:
 * </p>
 *
 * <pre>
 * import org.scalatest.WordSpec
 * 
 * class AndOrSpec extends WordSpec {
 * 
 *   "The ScalaTest Matchers DSL" should {
 *     "provide an and operator that returns silently when evaluating true and true" in {}
 *     "provide an and operator that throws a TestFailedException when evaluating true and false" in {}
 *     "provide an and operator that throws a TestFailedException when evaluating false and true" in {}
 *     "provide an and operator that throws a TestFailedException when evaluating false and false" in {}
 *     "provide an or operator that returns silently when evaluating true or true" in {}
 *     "provide an or operator that returns silently when evaluating true or false" in {}
 *     "provide an or operator that returns silently when evaluating false or true" in {}
 *     "provide an or operator that throws a TestFailedException when evaluating false or false" in {}
 *   }
 * }
 * </pre>
 *
 * <p>
 * In such situations you can place <code>that</code> clauses inside the verb clause, like this:
 * </p>
 *
 * <pre>
 * import org.scalatest.WordSpec
 * 
 * class AndOrSpec extends WordSpec {
 *
 *   "The ScalaTest Matchers DSL" should {
 *     "provide an and operator" that {
 *       "returns silently when evaluating true and true" in {}
 *       "throws a TestFailedException when evaluating true and false" in {}
 *       "that throws a TestFailedException when evaluating false and true" in {}
 *       "throws a TestFailedException when evaluating false and false" in {}
 *     }
 *     "provide an or operator" that {
 *       "returns silently when evaluating true or true" in {}
 *       "returns silently when evaluating true or false" in {}
 *       "returns silently when evaluating false or true" in {}
 *       "throws a TestFailedException when evaluating false or false" in {}
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * <a name="AfterWords">If</a> a word or phrase is repeated at the beginning of each string contained in a block, you can eliminate
 * that repetition by using an <em>after word</em>. An after word is a word or phrase that you can place
 * after <code>when</code>, a verb, or
 * <code>that</code>. For example, in the previous <code>WordSpec</code>, the word "provide" is repeated
 * at the beginning of each string inside the <code>should</code> block. You can factor out this duplication
 * like this:
 * </p>
 *
 * <pre>
 * import org.scalatest.WordSpec
 * 
 * class AndOrSpec extends WordSpec {
 * 
 *    def provide = afterWord("provide")
 * 
 *   "The ScalaTest Matchers DSL" should provide {
 *     "an and operator" that {
 *       "returns silently when evaluating true and true" in {}
 *       "throws a TestFailedException when evaluating true and false" in {}
 *       "that throws a TestFailedException when evaluating false and true" in {}
 *       "throws a TestFailedException when evaluating false and false" in {}
 *     }
 *     "an or operator" that {
 *       "returns silently when evaluating true or true" in {}
 *       "returns silently when evaluating true or false" in {}
 *       "returns silently when evaluating false or true" in {}
 *       "throws a TestFailedException when evaluating false or false" in {}
 *     }
 *   }
 * }
 * </pre>
 * 
 * <p>
 * Once you've defined an after word, you can place it after <code>when</code>, a verb
 * (<code>should</code>, <code>must</code>, or <code>can</code>), or
 * <code>that</code>. (You can't place one after <code>in</code> or <code>is</code>, the
 * words that introduce a test.) Here's an example that has after words used in all three
 * places:
 * </p>
 *
 * <pre>
 * import org.scalatest.WordSpec
 * 
 * class ScalaTestGUISpec extends WordSpec {
 * 
 *   def theUser = afterWord("the user")
 *   def display = afterWord("display")
 *   def is = afterWord("is")
 * 
 *   "The ScalaTest GUI" when theUser {
 *     "clicks on an event report in the list box" should display {
 *       "a blue background in the clicked-on row in the list box" in {}
 *       "the details for the event in the details area" in {}
 *       "a rerun button" that is {
 *         "enabled if the clicked-on event is rerunnable" in {}
 *         "disabled if the clicked-on event is not rerunnable" in {}
 *       }
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * Running the previous <code>WordSpec</code> in the Scala interpreter would yield:
 * </p>
 *
 * <pre>
 * scala> (new ScalaTestGUISpec).execute()
 * The ScalaTest GUI (when the user clicks on an event report in the list box) 
 * - should display a blue background in the clicked-on row in the list box
 * - should display the details for the event in the details area
 * - should display a rerun button that is enabled if the clicked-on event is rerunnable
 * - should display a rerun button that is disabled if the clicked-on event is not rerunnable
 * </pre>
 *
 * <p>
 * A <code>WordSpec</code>'s lifecycle has two phases: the <em>registration</em> phase and the
 * <em>ready</em> phase. It starts in registration phase and enters ready phase the first time
 * <code>run</code> is called on it. It then remains in ready phase for the remainder of its lifetime.
 * </p>
 *
 * <p>
 * Tests can only be registered while the <code>WordSpec</code> is
 * in its registration phase. Any attempt to register a test after the <code>WordSpec</code> has
 * entered its ready phase, <em>i.e.</em>, after <code>run</code> has been invoked on the <code>WordSpec</code>,
 * will be met with a thrown <code>TestRegistrationClosedException</code>. The recommended style
 * of using <code>WordSpec</code> is to register tests during object construction as is done in all
 * the examples shown here. If you keep to the recommended style, you should never see a
 * <code>TestRegistrationClosedException</code>.
 * </p>
 *
 * <p>
 * <strong>Shared fixtures</strong>
 * </p>
 *
 * <p>
 * A test <em>fixture</em> is objects or other artifacts (such as files, sockets, database
 * connections, etc.) used by tests to do their work. You can use fixtures in
 * <code>WordSpec</code>s with the same approaches suggested for <code>Suite</code> in
 * its documentation. The same text that appears in the test fixture
 * section of <code>Suite</code>'s documentation is repeated here, with examples changed from
 * <code>Suite</code> to <code>WordSpec</code>.
 * </p>
 *
 * <p>
 * If a fixture is used by only one test, then the definitions of the fixture objects can
 * be local to the test function, such as the objects assigned to <code>stack</code> and <code>emptyStack</code> in the
 * previous <code>StackSpec</code> examples. If multiple tests need to share an immutable fixture, one approach
 * is to assign them to instance variables. Here's a (very contrived) example, in which the object assigned
 * to <code>shared</code> is used by multiple test functions:
 * </p>
 *
 * <pre>
 * import org.scalatest.WordSpec
 *
 * class ArithmeticSpec extends WordSpec {
 *
 *   // Sharing immutable fixture objects via instance variables
 *   val shared = 5
 *
 *  "The Scala language" should {
 *     "add correctly" in {
 *       val sum = 2 + 3
 *       assert(sum === shared)
 *     }
 *
 *     "subtract correctly" in {
 *       val diff = 7 - 2
 *       assert(diff === shared)
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * In some cases, however, shared <em>mutable</em> fixture objects may be changed by tests such that
 * they need to be recreated or reinitialized before each test. Shared resources such
 * as files or database connections may also need to be created and initialized before, and
 * cleaned up after, each test. JUnit offers methods <code>setUp</code> and
 * <code>tearDown</code> for this purpose. In ScalaTest, you can use the <code>BeforeAndAfterEach</code> trait,
 * which will be described later, to implement an approach similar to JUnit's <code>setUp</code>
 * and <code>tearDown</code>, however, this approach often involves reassigning <code>var</code>s
 * between tests. Before going that route, you should consider some approaches that
 * avoid <code>var</code>s. One approach is to write one or more <em>create-fixture</em> methods
 * that return a new instance of a needed object (or a tuple or case class holding new instances of
 * multiple objects) each time it is called. You can then call a create-fixture method at the beginning of each
 * test that needs the fixture, storing the fixture object or objects in local variables. Here's an example:
 * </p>
 *
 * <pre>
 * import org.scalatest.WordSpec
 * import scala.collection.mutable.ListBuffer
 *
 * class MySuite extends WordSpec {
 *
 *   // create objects needed by tests and return as a tuple
 *   def createFixture = (
 *     new StringBuilder("ScalaTest is "),
 *     new ListBuffer[String]
 *   )
 *
 *  "ScalaTest" should {
 *
 *     "be easy " in {
 *       val (builder, lbuf) = createFixture
 *       builder.append("easy!")
 *       assert(builder.toString === "ScalaTest is easy!")
 *       assert(lbuf.isEmpty)
 *       lbuf += "sweet"
 *     }
 *
 *     "be fun" in {
 *       val (builder, lbuf) = createFixture
 *       builder.append("fun!")
 *       assert(builder.toString === "ScalaTest is fun!")
 *       assert(lbuf.isEmpty)
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * If different tests in the same <code>WordSpec</code> require different fixtures, you can create multiple create-fixture methods and
 * call the method (or methods) needed by each test at the begining of the test. If every test requires the same set of
 * mutable fixture objects, one other approach you can take is make them simply <code>val</code>s and mix in trait
 * <a href="OneInstancePerTest.html"><code>OneInstancePerTest</code></a>.  If you mix in <code>OneInstancePerTest</code>, each test
 * will be run in its own instance of the <code>WordSpec</code>, similar to the way JUnit tests are executed.
 * </p>
 *
 * <p>
 * Although the create-fixture and <code>OneInstancePerTest</code> approaches take care of setting up a fixture before each
 * test, they don't address the problem of cleaning up a fixture after the test completes. In this situation,
 * one option is to mix in the <a href="BeforeAndAfterEach.html"><code>BeforeAndAfterEach</code></a> trait.
 * <code>BeforeAndAfterEach</code>'s <code>beforeEach</code> method will be run before, and its <code>afterEach</code>
 * method after, each test (like JUnit's <code>setUp</code>  and <code>tearDown</code>
 * methods, respectively). 
 * For example, you could create a temporary file before each test, and delete it afterwords, like this:
 * </p>
 *
 * <pre>
 * import org.scalatest.WordSpec
 * import org.scalatest.BeforeAndAfterEach
 * import java.io.FileReader
 * import java.io.FileWriter
 * import java.io.File
 *
 * class MySuite extends WordSpec with BeforeAndAfterEach {
 *
 *   private val FileName = "TempFile.txt"
 *   private var reader: FileReader = _
 *
 *   // Set up the temp file needed by the test
 *   override def beforeEach() {
 *     val writer = new FileWriter(FileName)
 *     try {
 *       writer.write("Hello, test!")
 *     }
 *     finally {
 *       writer.close()
 *     }
 *
 *     // Create the reader needed by the test
 *     reader = new FileReader(FileName)
 *   }
 *
 *   // Close and delete the temp file
 *   override def afterEach() {
 *     reader.close()
 *     val file = new File(FileName)
 *     file.delete()
 *   }
 *
 *  "A FileReader" must {
 *     "read in the contents of a file correctly" in {
 *       var builder = new StringBuilder
 *       var c = reader.read()
 *       while (c != -1) {
 *         builder.append(c.toChar)
 *         c = reader.read()
 *       }
 *       assert(builder.toString === "Hello, test!")
 *     }
 * 
 *     "read in the first character of a file correctly" in {
 *       assert(reader.read() === 'H')
 *     }
 *
 *     "not be required" in {
 *       assert(1 + 1 === 2)
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * In this example, the instance variable <code>reader</code> is a <code>var</code>, so
 * it can be reinitialized between tests by the <code>beforeEach</code> method.
 * </p>
 * 
 * <p>
 * Although the <code>BeforeAndAfterEach</code> approach should be familiar to the users of most
 * test other frameworks, ScalaTest provides another alternative that also allows you to perform cleanup
 * after each test: overriding <code>withFixture(NoArgTest)</code>.
 * To execute each test, <code>Suite</code>'s implementation of the <code>runTest</code> method wraps an invocation
 * of the appropriate test method in a no-arg function. <code>runTest</code> passes that test function to the <code>withFixture(NoArgTest)</code>
 * method, which is responsible for actually running the test by invoking the function. <code>Suite</code>'s
 * implementation of <code>withFixture(NoArgTest)</code> simply invokes the function, like this:
 * </p>
 *
 * <pre>
 * // Default implementation
 * protected def withFixture(test: NoArgTest) {
 *   test()
 * }
 * </pre>
 *
 * <p>
 * The <code>withFixture(NoArgTest)</code> method exists so that you can override it and set a fixture up before, and clean it up after, each test.
 * Thus, the previous temp file example could also be implemented without mixing in <code>BeforeAndAfterEach</code>, like this:
 * </p>
 *
 * <pre>
 * import org.scalatest.WordSpec
 * import org.scalatest.BeforeAndAfterEach
 * import java.io.FileReader
 * import java.io.FileWriter
 * import java.io.File
 *
 * class MySuite extends WordSpec {
 *
 *   private var reader: FileReader = _
 *
 *   override def withFixture(test: NoArgTest) {
 *
 *     val FileName = "TempFile.txt"
 *
 *     // Set up the temp file needed by the test
 *     val writer = new FileWriter(FileName)
 *     try {
 *       writer.write("Hello, test!")
 *     }
 *     finally {
 *       writer.close()
 *     }
 *
 *     // Create the reader needed by the test
 *     reader = new FileReader(FileName)
 *
 *     try {
 *       test() // Invoke the test function
 *     }
 *     finally {
 *       // Close and delete the temp file
 *       reader.close()
 *       val file = new File(FileName)
 *       file.delete()
 *     }
 *   }
 *
 *  "A FileReader" must {
 *     "read in the contents of a file correctly" in {
 *       var builder = new StringBuilder
 *       var c = reader.read()
 *       while (c != -1) {
 *         builder.append(c.toChar)
 *         c = reader.read()
 *       }
 *       assert(builder.toString === "Hello, test!")
 *     }
 * 
 *     "read in the first character of a file correctly" in {
 *       assert(reader.read() === 'H')
 *     }
 *
 *     "not be required" in {
 *       assert(1 + 1 === 2)
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you prefer to keep your test classes immutable, one final variation is to use the
 * <a href="fixture/FixtureWordSpec.html"><code>FixtureWordSpec</code></a> trait from the
 * <code>org.scalatest.fixture</code> package.  Tests in an <code>org.scalatest.fixture.FixtureWordSpec</code> can have a fixture
 * object passed in as a parameter. You must indicate the type of the fixture object
 * by defining the <code>Fixture</code> type member and define a <code>withFixture</code> method that takes a <em>one-arg</em> test function.
 * (A <code>FixtureWordSpec</code> has two overloaded <code>withFixture</code> methods, therefore, one that takes a <code>OneArgTest</code>
 * and the other, inherited from <code>Suite</code>, that takes a <code>NoArgTest</code>.)
 * Inside the <code>withFixture(OneArgTest)</code> method, you create the fixture, pass it into the test function, then perform any
 * necessary cleanup after the test function returns. Instead of invoking each test directly, a <code>FixtureWordSpec</code> will
 * pass a function that invokes the code of a test to <code>withFixture(OneArgTest)</code>. Your <code>withFixture(OneArgTest)</code> method, therefore,
 * is responsible for actually running the code of the test by invoking the test function.
 * For example, you could pass the temp file reader fixture to each test that needs it
 * by overriding the <code>withFixture(OneArgTest)</code> method of a <code>FixtureWordSpec</code>, like this:
 * </p>
 *
 * <pre>
 * import org.scalatest.fixture.FixtureWordSpec
 * import java.io.FileReader
 * import java.io.FileWriter
 * import java.io.File
 * 
 * class MySuite extends FixtureWordSpec {
 *
 *   type FixtureParam = FileReader
 *
 *   def withFixture(test: OneArgTest) {
 *
 *     val FileName = "TempFile.txt"
 *
 *     // Set up the temp file needed by the test
 *     val writer = new FileWriter(FileName)
 *     try {
 *       writer.write("Hello, test!")
 *     }
 *     finally {
 *       writer.close()
 *     }
 *
 *     // Create the reader needed by the test
 *     val reader = new FileReader(FileName)
 *  
 *     try {
 *       // Run the test using the temp file
 *       test(reader)
 *     }
 *     finally {
 *       // Close and delete the temp file
 *       reader.close()
 *       val file = new File(FileName)
 *       file.delete()
 *     }
 *   }
 * 
 *  "A FileReader" must {
 *     "read in the contents of a file correctly" in { reader =>
 *       var builder = new StringBuilder
 *       var c = reader.read()
 *       while (c != -1) {
 *         builder.append(c.toChar)
 *         c = reader.read()
 *       }
 *       assert(builder.toString === "Hello, test!")
 *     }
 * 
 *     "read in the first character of a file correctly" in { reader =>
 *       assert(reader.read() === 'H')
 *     }
 *
 *     "not be required" in { () =>
 *       assert(1 + 1 === 2)
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * It is worth noting that the only difference in the test code between the mutable
 * <code>BeforeAndAfterEach</code> approach shown here and the immutable <code>FixtureWordSpec</code>
 * approach shown previously is that two of the <code>FixtureWordSpec</code>'s test functions take a <code>FileReader</code> as
 * a parameter via the "<code>reader =></code>" at the beginning of the function. Otherwise the test code is identical.
 * One benefit of the explicit parameter is that, as demonstrated
 * by the "<code>A FileReader must not be required</code>" test, a <code>FixtureWordSpec</code>
 * test need not take the fixture. So you can have some tests that take a fixture, and others that don't.
 * In this case, the <code>FixtureWordSpec</code> provides documentation indicating which
 * tests use the fixture and which don't, whereas the <code>BeforeAndAfterEach</code> approach does not.
 * (If you have want to combine tests that take different fixture types in the same <code>WordSpec</code>, you can
 * use <a href="fixture/MultipleFixtureWordSpec.html">MultipleFixtureWordSpec</a>.)
 * </p>
 *
 * <p>
 * If you want to execute code before and after all tests (and nested suites) in a suite, such
 * as you could do with <code>@BeforeClass</code> and <code>@AfterClass</code>
 * annotations in JUnit 4, you can use the <code>beforeAll</code> and <code>afterAll</code>
 * methods of <code>BeforeAndAfterAll</code>. See the documentation for <code>BeforeAndAfterAll</code> for
 * an example.
 * </p>
 *
 * <p>
 * <a name="SharedTests"><strong>Shared tests</strong></a>
 * </p>
 *
 * <p>
 * Sometimes you may want to run the same test code on different fixture objects. In other words, you may want to write tests that are "shared"
 * by different fixture objects.  To accomplish this in a <code>WordSpec</code>, you first place shared tests in <em>behavior functions</em>.
 * These behavior functions will be invoked during the construction phase of any <code>WordSpec</code> that uses them, so that the tests they
 * contain will be registered as tests in that <code>WordSpec</code>.  For example, given this stack class:
 * </p>
 *
 * <pre>
 * import scala.collection.mutable.ListBuffer
 * 
 * class Stack[T] {
 *
 *   val MAX = 10
 *   private var buf = new ListBuffer[T]
 *
 *   def push(o: T) {
 *     if (!full)
 *       o +: buf
 *     else
 *       throw new IllegalStateException("can't push onto a full stack")
 *   }
 *
 *   def pop(): T = {
 *     if (!empty)
 *       buf.remove(0)
 *     else
 *       throw new IllegalStateException("can't pop an empty stack")
 *   }
 *
 *   def peek: T = {
 *     if (!empty)
 *       buf(0)
 *     else
 *       throw new IllegalStateException("can't pop an empty stack")
 *   }
 *
 *   def full: Boolean = buf.size == MAX
 *   def empty: Boolean = buf.size == 0
 *   def size = buf.size
 *
 *   override def toString = buf.mkString("Stack(", ", ", ")")
 * }
 * </pre>
 *
 * <p>
 * You may want to test the <code>Stack</code> class in different states: empty, full, with one item, with one item less than capacity,
 * <em>etc</em>. You may find you have several tests that make sense any time the stack is non-empty. Thus you'd ideally want to run
 * those same tests for three stack fixture objects: a full stack, a stack with a one item, and a stack with one item less than
 * capacity. With shared tests, you can factor these tests out into a behavior function, into which you pass the
 * stack fixture to use when running the tests. So in your <code>WordSpec</code> for stack, you'd invoke the
 * behavior function three times, passing in each of the three stack fixtures so that the shared tests are run for all three fixtures. You
 * can define a behavior function that encapsulates these shared tests inside the <code>WordSpec</code> that uses them. If they are shared
 * between different <code>WordSpec</code>s, however, you could also define them in a separate trait that is mixed into each <code>WordSpec</code>
 * that uses them.
 * </p>
 *
 * <p>
 * <a name="StackBehaviors">For</a> example, here the <code>nonEmptyStack</code> behavior function (in this case, a behavior <em>method</em>) is
 * defined in a trait along with another method containing shared tests for non-full stacks:
 * </p>
 * 
 * <pre>
 * trait StackBehaviors { this: WordSpec =>
 * 
 *   def nonEmptyStack(stack: Stack[Int], lastItemAdded: Int) {
 * 
 *     "be non-empty" in {
 *       assert(!stack.empty)
 *     }  
 * 
 *     "return the top item on peek" in {
 *       assert(stack.peek === lastItemAdded)
 *     }
 *   
 *     "not remove the top item on peek" in {
 *       val size = stack.size
 *       assert(stack.peek === lastItemAdded)
 *       assert(stack.size === size)
 *     }
 *   
 *     "remove the top item on pop" in {
 *       val size = stack.size
 *       assert(stack.pop === lastItemAdded)
 *       assert(stack.size === size - 1)
 *     }
 *   }
 *   
 *   def nonFullStack(stack: Stack[Int]) {
 *       
 *     "not be full" in {
 *       assert(!stack.full)
 *     }
 *       
 *     "add to the top on push" in {
 *       val size = stack.size
 *       stack.push(7)
 *       assert(stack.size === size + 1)
 *       assert(stack.peek === 7)
 *     }
 *   }
 * }
 * </pre>
 *
 *
 * <p>
 * Given these behavior functions, you could invoke them directly, but <code>WordSpec</code> offers a DSL for the purpose,
 * which looks like this:
 * </p>
 *
 * <pre>
 * behave like nonEmptyStack(stackWithOneItem, lastValuePushed)
 * behave like nonFullStack(stackWithOneItem)
 * </pre>
 *
 * <p>
 * If you prefer to use an imperative style to change fixtures, for example by mixing in <code>BeforeAndAfterEach</code> and
 * reassigning a <code>stack</code> <code>var</code> in <code>beforeEach</code>, you could write your behavior functions
 * in the context of that <code>var</code>, which means you wouldn't need to pass in the stack fixture because it would be
 * in scope already inside the behavior function. In that case, your code would look like this:
 * </p>
 *
 * <pre>
 * behave like nonEmptyStack // assuming lastValuePushed is also in scope inside nonEmptyStack
 * behave like nonFullStack
 * </pre>
 *
 * <p>
 * The recommended style, however, is the functional, pass-all-the-needed-values-in style. Here's an example:
 * </p>
 *
 * <pre>
 * class SharedTestExampleSpec extends WordSpec with StackBehaviors {
 * 
 *   // Stack fixture creation methods
 *   def emptyStack = new Stack[Int]
 * 
 *   def fullStack = {
 *     val stack = new Stack[Int]
 *     for (i <- 0 until stack.MAX)
 *       stack.push(i)
 *     stack
 *   }
 * 
 *   def stackWithOneItem = {
 *     val stack = new Stack[Int]
 *     stack.push(9)
 *     stack
 *   }
 * 
 *   def stackWithOneItemLessThanCapacity = {
 *     val stack = new Stack[Int]
 *     for (i <- 1 to 9)
 *       stack.push(i)
 *     stack
 *   }
 * 
 *   val lastValuePushed = 9
 * 
 *   "A Stack" when {
 *     "empty" should {
 *       "be empty" in {
 *         assert(emptyStack.empty)
 *       }
 * 
 *       "complain on peek" in {
 *         intercept[IllegalStateException] {
 *           emptyStack.peek
 *         }
 *       }
 *
 *       "complain on pop" in {
 *         intercept[IllegalStateException] {
 *           emptyStack.pop
 *         }
 *       }
 *     }
 * 
 *     "it contains one item" should {
 *       behave like nonEmptyStack(stackWithOneItem, lastValuePushed)
 *       behave like nonFullStack(stackWithOneItem)
 *     }
 *
 *     
 *     "it contains one item less than capacity" should {
 *       behave like nonEmptyStack(stackWithOneItemLessThanCapacity, lastValuePushed)
 *       behave like nonFullStack(stackWithOneItemLessThanCapacity)
 *     }
 * 
 *     "full" should {
 *       "be full" in {
 *         assert(fullStack.full)
 *       }
 * 
 *       behave like nonEmptyStack(fullStack, lastValuePushed)
 * 
 *       "complain on a push" in {
 *         intercept[IllegalStateException] {
 *           fullStack.push(10)
 *         }
 *       }
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you load these classes into the Scala interpreter (with scalatest's JAR file on the class path), and execute it,
 * you'll see:
 * </p>
 *
 * <pre>
 * scala> (new SharedTestExampleSpec).execute()
 * A Stack (when empty) 
 * - should be empty
 * - should complain on peek
 * - should complain on pop
 * A Stack (when it contains one item) 
 * - should be non-empty
 * - should return the top item on peek
 * - should not remove the top item on peek
 * - should remove the top item on pop
 * - should not be full
 * - should add to the top on push
 * A Stack (when it contains one item less than capacity) 
 * - should be non-empty
 * - should return the top item on peek
 * - should not remove the top item on peek
 * - should remove the top item on pop
 * - should not be full
 * - should add to the top on push
 * A Stack (when full) 
 * - should be full
 * - should be non-empty
 * - should return the top item on peek
 * - should not remove the top item on peek
 * - should remove the top item on pop
 * - should complain on a push
 * </pre>
 * 
 * <p>
 * One thing to keep in mind when using shared tests is that in ScalaTest, each test in a suite must have a unique name.
 * If you register the same tests repeatedly in the same suite, one problem you may encounter is an exception at runtime
 * complaining that multiple tests are being registered with the same test name. A good way to solve this problem in a <code>FlatSpec</code> is to make sure
 * each invocation of a behavior function is in the context of a different <code>behavior of</code> clause, which will prepend a string to each test name.
 * For example, the following code in a <code>FlatSpec</code> would register a test with the name <code>"A Stack (when empty) should be empty"</code>:
 * </p>
 *
 * <pre>
 *   behavior of "A Stack (when empty)"
 *       
 *   it should "be empty" in {
 *     assert(emptyStack.empty)
 *   }
 *   // ...
 * </pre>
 *
 * <p>
 * Or, using the shorthand notation:
 * </p>
 *
 * <pre>
 *   "A Stack (when empty)" should "be empty" in {
 *     assert(emptyStack.empty)
 *   }
 *   // ...
 * </pre>
 *
 * <p>
 * If the <code>"should be empty"</code> test was factored out into a behavior function, it could be called repeatedly so long
 * as each invocation of the behavior function is in the context of a different <code>behavior of</code> clause.
 * </p>
 *
 * <p>
 * <strong>Tagging tests</strong>
 * </p>
 *
 * A <code>WordSpec</code>'s tests may be classified into groups by <em>tagging</em> them with string names.
 * As with any suite, when executing a <code>WordSpec</code>, groups of tests can
 * optionally be included and/or excluded. To tag a <code>WordSpec</code>'s tests,
 * you pass objects that extend abstract class <code>org.scalatest.Tag</code> to <code>taggedAs</code> method
 * invoked on the string that describes the test you want to tag. Class <code>Tag</code> takes one parameter,
 * a string name.  If you have
 * created Java annotation interfaces for use as group names in direct subclasses of <code>org.scalatest.Suite</code>,
 * then you will probably want to use group names on your <code>WordSpec</code>s that match. To do so, simply 
 * pass the fully qualified names of the Java interfaces to the <code>Tag</code> constructor. For example, if you've
 * defined Java annotation interfaces with fully qualified names, <code>com.mycompany.groups.SlowTest</code> and <code>com.mycompany.groups.DbTest</code>, then you could
 * create matching groups for <code>Spec</code>s like this:
 * </p>
 *
 * <pre>
 * import org.scalatest.Tag
 *
 * object SlowTest extends Tag("com.mycompany.groups.SlowTest")
 * object DbTest extends Tag("com.mycompany.groups.DbTest")
 * </pre>
 *
 * <p>
 * Given these definitions, you could place <code>WordSpec</code> tests into groups like this:
 * </p>
 *
 * <pre>
 * import org.scalatest.WordSpec
 *
 * class MySuite extends WordSpec {
 *
 *   "The Scala language" should {
 *
 *     "add correctly" taggedAs(SlowTest) in {
 *       val sum = 1 + 1
 *       assert(sum === 2)
 *       assert(sum + 2 === 4)
 *     }
 *
 *     "subtract correctly" taggedAs(SlowTest, DbTest) in {
 *       val diff = 4 - 1
 *       assert(diff === 3)
 *       assert(diff - 2 === 1)
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * This code marks both tests with the <code>com.mycompany.groups.SlowTest</code> tag, 
 * and test <code>"The Scala language should subtract correctly"</code> with the <code>com.mycompany.groups.DbTest</code> tag.
 * </p>
 *
 * <p>
 * The primary <code>run</code> method takes a <code>Filter</code>, whose constructor takes an optional
 * <code>Set[String]</code>s called <code>tagsToInclude</code> and a <code>Set[String]</code> called
 * <code>tagsToExclude</code>. If <code>tagsToInclude</code> is <code>None</code>, all tests will be run
 * except those those belonging to tags listed in the
 * <code>tagsToExclude</code> <code>Set</code>. If <code>tagsToInclude</code> is defined, only tests
 * belonging to tags mentioned in the <code>tagsToInclude</code> set, and not mentioned in <code>tagsToExclude</code>,
 * will be run.
 * </p>
 *
 * <p>
 * <strong>Ignored tests</strong>
 * </p>
 *
 * To support the common use case of &#8220;temporarily&#8221; disabling a test, with the
 * good intention of resurrecting the test at a later time, <code>WordSpec</code> adds a method
 * <code>ignore</code> to strings that can be used instead of <code>in</code> to register a test. For example, to temporarily
 * disable the test with the name <code>"A Stack should pop values in last-in-first-out order"</code>, just
 * change &#8220;<code>in</code>&#8221; into &#8220;<code>ignore</code>,&#8221; like this:
 * </p>
 *
 * <pre>
 * import org.scalatest.WordSpec
 * import scala.collection.mutable.Stack
 *
 * class StackSpec extends WordSpec {
 *
 *   "A Stack" should {
 *
 *     "pop values in last-in-first-out order" ignore {
 *       val stack = new Stack[Int]
 *       stack.push(1)
 *       stack.push(2)
 *       assert(stack.pop() === 2)
 *       assert(stack.pop() === 1)
 *     }
 *
 *     "throw NoSuchElementException if an empty stack is popped" in {
 *       val emptyStack = new Stack[String]
 *       intercept[NoSuchElementException] {
 *         emptyStack.pop()
 *       }
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you run this version of <code>StackSpec</code> with:
 * </p>
 *
 * <pre>
 * scala> (new StackSpec).execute()
 * </pre>
 *
 * <p>
 * It will run only the second test and report that the first test was ignored:
 * </p>
 *
 * <pre>
 * A Stack
 * - should pop values in last-in-first-out order !!! IGNORED !!!
 * - should throw NoSuchElementException if an empty stack is popped
 * </pre>
 *
 * <p>
 * <strong>Informers</strong>
 * </p>
 *
 * <p>
 * One of the parameters to the primary <code>run</code> method is a <code>Reporter</code>, which
 * will collect and report information about the running suite of tests.
 * Information about suites and tests that were run, whether tests succeeded or failed, 
 * and tests that were ignored will be passed to the <code>Reporter</code> as the suite runs.
 * Most often the reporting done by default by <code>WordSpec</code>'s methods will be sufficient, but
 * occasionally you may wish to provide custom information to the <code>Reporter</code> from a test.
 * For this purpose, an <code>Informer</code> that will forward information to the current <code>Reporter</code>
 * is provided via the <code>info</code> parameterless method.
 * You can pass the extra information to the <code>Informer</code> via its <code>apply</code> method.
 * The <code>Informer</code> will then pass the information to the <code>Reporter</code> via an <code>InfoProvided</code> event.
 * Here's an example:
 * </p>
 *
 * <pre>
 * import org.scalatest.WordSpec
 *
 * class ArithmeticSpec extends WordSpec {
 *
 *  "The Scala language" should {
 *     "add correctly" in {
 *       val sum = 2 + 3
 *       assert(sum === 5)
 *       info("addition seems to work")
 *     }
 *
 *     "subtract correctly" in {
 *       val diff = 7 - 2
 *       assert(diff === 5)
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you run this <code>WordSpec</code> from the interpreter, you will see the following message
 * included in the printed report:
 * </p>
 *
 * <pre>
 * scala> (new ArithmeticSpec).execute()
 * The Scala language 
 * - should add correctly
 *   + addition seems to work 
 * - should subtract correctly
 * </pre>
 *
 * <p>
 * One use case for the <code>Informer</code> is to pass more information about a specification to the reporter. For example,
 * the <code>GivenWhenThen</code> trait provides methods that use the implicit <code>info</code> provided by <code>WordSpec</code>
 * to pass such information to the reporter. Here's an example:
 * </p>
 *
 * <pre>
 * import org.scalatest.WordSpec
 * import org.scalatest.GivenWhenThen
 * 
 * class ArithmeticSpec extends WordSpec with GivenWhenThen {
 * 
 *  "The Scala language" should {
 * 
 *     "add correctly" in { 
 * 
 *       given("two integers")
 *       val x = 2
 *       val y = 3
 * 
 *       when("they are added")
 *       val sum = x + y
 * 
 *       then("the result is the sum of the two numbers")
 *       assert(sum === 5)
 *     }
 * 
 *     "subtract correctly" in {
 * 
 *       given("two integers")
 *       val x = 7
 *       val y = 2
 * 
 *       when("one is subtracted from the other")
 *       val diff = x - y
 * 
 *       then("the result is the difference of the two numbers")
 *       assert(diff === 5)
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you run this <code>WordSpec</code> from the interpreter, you will see the following messages
 * included in the printed report:
 * </p>
 *
 * <pre>
 * scala> (new ArithmeticSpec).execute()
 * The Scala language 
 * - should add correctly
 *   + Given two integers 
 *   + When they are added 
 *   + Then the result is the sum of the two numbers 
 * - should subtract correctly
 *   + Given two integers 
 *   + When one is subtracted from the other 
 *   + Then the result is the difference of the two numbers 
 * </pre>
 *
 * <p>
 * <strong>Pending tests</strong>
 * </p>
 *
 * <p>
 * A <em>pending test</em> is one that has been given a name but is not yet implemented. The purpose of
 * pending tests is to facilitate a style of testing in which documentation of behavior is sketched
 * out before tests are written to verify that behavior (and often, the before the behavior of
 * the system being tested is itself implemented). Such sketches form a kind of specification of
 * what tests and functionality to implement later.
 * </p>
 *
 * <p>
 * To support this style of testing, a test can be given a name that specifies one
 * bit of behavior required by the system being tested. The test can also include some code that
 * sends more information about the behavior to the reporter when the tests run. At the end of the test,
 * it can call method <code>pending</code>, which will cause it to complete abruptly with <code>TestPendingException</code>.
 * Because tests in ScalaTest can be designated as pending with <code>TestPendingException</code>, both the test name and any information
 * sent to the reporter when running the test can appear in the report of a test run. (In other words,
 * the code of a pending test is executed just like any other test.) However, because the test completes abruptly
 * with <code>TestPendingException</code>, the test will be reported as pending, to indicate
 * the actual test, and possibly the functionality it is intended to test, has not yet been implemented.
 * You can mark tests as pending in a <code>WordSpec</code> like this:
 * </p>
 *
 * <pre>
 * import org.scalatest.WordSpec
 *
 * class ArithmeticSpec extends WordSpec {
 *
 *   // Sharing fixture objects via instance variables
 *   val shared = 5
 *
 *  "The Scala language" should {
 *     "add correctly" in {
 *       val sum = 2 + 3
 *       assert(sum === shared)
 *     }
 *
 *     "subtract correctly" is (pending)
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you run this version of <code>ArithmeticSpec</code> with:
 * </p>
 *
 * <pre>
 * scala> (new ArithmeticSpec).execute()
 * </pre>
 *
 * <p>
 * It will run both tests but report that <code>The Scala language should subtract correctly</code> is pending. You'll see:
 * </p>
 *
 * <pre>
 * The Scala language
 * - should add correctly
 * - should subtract correctly (pending)
 * </pre>
 * 
 * <p>
 * One difference between an ignored test and a pending one is that an ignored test is intended to be used during a
 * significant refactorings of the code under test, when tests break and you don't want to spend the time to fix
 * all of them immediately. You can mark some of those broken tests as ignored temporarily, so that you can focus the red
 * bar on just failing tests you actually want to fix immediately. Later you can go back and fix the ignored tests.
 * In other words, by ignoring some failing tests temporarily, you can more easily notice failed tests that you actually
 * want to fix. By contrast, a pending test is intended to be used before a test and/or the code under test is written.
 * Pending indicates you've decided to write a test for a bit of behavior, but either you haven't written the test yet, or
 * have only written part of it, or perhaps you've written the test but don't want to implement the behavior it tests
 * until after you've implemented a different bit of behavior you realized you need first. Thus ignored tests are designed
 * to facilitate refactoring of existing code whereas pending tests are designed to facilitate the creation of new code.
 * </p>
 *
 * <p>
 * One other difference between ignored and pending tests is that ignored tests are implemented as a test tag that is
 * excluded by default. Thus an ignored test is never executed. By contrast, a pending test is implemented as a
 * test that throws <code>TestPendingException</code> (which is what calling the <code>pending</code> method does). Thus
 * the body of pending tests are executed up until they throw <code>TestPendingException</code>. The reason for this difference
 * is that it enables your unfinished test to send <code>InfoProvided</code> messages to the reporter before it completes
 * abruptly with <code>TestPendingException</code>, as shown in the previous example on <code>Informer</code>s
 * that used the <code>GivenWhenThen</code> trait. For example, the following snippet in a <code>WordSpec</code>:
 * </p>
 *
 * <pre>
 *  "The Scala language" should {
 *     "add correctly" in { 
 *       given("two integers")
 *       when("they are added")
 *       then("the result is the sum of the two numbers")
 *       pending
 *     }
 *     // ...
 * </pre>
 *
 * <p>
 * Would yield the following output when run in the interpreter:
 * </p>
 *
 * <pre>
 * The Scala language
 * - should add correctly (pending)
 *   + Given two integers 
 *   + When they are added 
 *   + Then the result is the sum of the two numbers 
 * </pre>
 *
 * @author Bill Venners
 */
trait WordSpec extends Suite with ShouldVerb with MustVerb with CanVerb { thisSuite =>

  private val IgnoreTagName = "org.scalatest.Ignore"

  private class Bundle private(
    val trunk: Trunk,
    val currentBranch: Branch,
    val tagsMap: Map[String, Set[String]],

    // All tests, in reverse order of registration
    val testsList: List[TestLeaf],

    // Used to detect at runtime that they've stuck a describe or an it inside an it,
    // which should result in a TestRegistrationClosedException
    val registrationClosed: Boolean
  ) {
    def unpack = (trunk, currentBranch, tagsMap, testsList, registrationClosed)
  }

  private object Bundle {
    def apply(
      trunk: Trunk,
      currentBranch: Branch,
      tagsMap: Map[String, Set[String]],
      testsList: List[TestLeaf],
      registrationClosed: Boolean
    ): Bundle =
      new Bundle(trunk, currentBranch, tagsMap, testsList, registrationClosed)

    def initialize(
      trunk: Trunk,
      tagsMap: Map[String, Set[String]],
      testsList: List[TestLeaf],
      registrationClosed: Boolean
    ): Bundle =
      new Bundle(trunk, trunk, tagsMap, testsList, registrationClosed)
  }

  private val atomic =
    new AtomicReference[Bundle](
      Bundle.initialize(new Trunk, Map(), List[TestLeaf](), false)
    )

  private def updateAtomic(oldBundle: Bundle, newBundle: Bundle) {
    val shouldBeOldBundle = atomic.getAndSet(newBundle)
    if (!(shouldBeOldBundle eq oldBundle))
      throw new ConcurrentModificationException(Resources("concurrentWordSpecBundleMod"))
  }

  private def registerTest(specText: String, f: () => Unit) = {

    val oldBundle = atomic.get
    var (trunk, currentBranch, tagsMap, testsList, registrationClosed) = oldBundle.unpack

    val testName = getTestName(specText, currentBranch)
    if (testsList.exists(_.testName == testName)) {
      throw new DuplicateTestNameException(testName, getStackDepth("Spec.scala", "it"))
    }
    val testShortName = specText
    val test = TestLeaf(currentBranch, testName, specText, f)
    currentBranch.subNodes ::= test
    testsList ::= test

    updateAtomic(oldBundle, Bundle(trunk, currentBranch, tagsMap, testsList, registrationClosed))

    testName
  }

  private class RegistrationInformer extends Informer {
    def apply(message: String) {
      if (message == null)
        throw new NullPointerException

      val oldBundle = atomic.get
      var (trunk, currentBranch, tagsMap, testsList, registrationClosed) = oldBundle.unpack

      currentBranch.subNodes ::= InfoLeaf(currentBranch, message)

      updateAtomic(oldBundle, Bundle(trunk, currentBranch, tagsMap, testsList, registrationClosed))
    }
  }

  // The informer will be a registration informer until run is called for the first time. (This
  // is the registration phase of a WordSpec's lifecycle.)
  private final val atomicInformer = new AtomicReference[Informer](new RegistrationInformer)

  /**
   * Returns an <code>Informer</code> that during test execution will forward strings (and other objects) passed to its
   * <code>apply</code> method to the current reporter. If invoked in a constructor, it
   * will register the passed string for forwarding later during test execution. If invoked while this
   * <code>WordSpec</code> is being executed, such as from inside a test function, it will forward the information to
   * the current reporter immediately. If invoked at any other time, it will
   * throw an exception. This method can be called safely by any thread.
   */
  implicit protected def info: Informer = atomicInformer.get

  private val zombieInformer =
    new Informer {
      private val complaint = Resources("cantCallInfoNow", "WordSpec")
      def apply(message: String) {
        if (message == null)
          throw new NullPointerException
        throw new IllegalStateException(complaint)
      }
    }

  /**
   * Register a test with the given spec text, optional tags, and test function value that takes no arguments.
   * An invocation of this method is called an &#8220;example.&#8221;
   *
   * This method will register the test for later execution via an invocation of one of the <code>execute</code>
   * methods. The name of the test will be a concatenation of the text of all surrounding describers,
   * from outside in, and the passed spec text, with one space placed between each item. (See the documenation
   * for <code>testNames</code> for an example.) The resulting test name must not have been registered previously on
   * this <code>Spec</code> instance.
   *
   * @param specText the specification text, which will be combined with the descText of any surrounding describers
   * to form the test name
   * @param testTags the optional list of tags for this test
   * @param testFun the test function
   * @throws DuplicateTestNameException if a test with the same name has been registered previously
   * @throws TestRegistrationClosedException if invoked after <code>run</code> has been invoked on this suite
   * @throws NullPointerException if <code>specText</code> or any passed test tag is <code>null</code>
   */
  private def registerTestToRun(specText: String, testTags: List[Tag], testFun: () => Unit) {

    if (atomic.get.registrationClosed)
      throw new TestRegistrationClosedException(Resources("itCannotAppearInsideAnotherIt"), getStackDepth("Spec.scala", "it"))
    if (specText == null)
      throw new NullPointerException("specText was null")
    if (testTags.exists(_ == null))
      throw new NullPointerException("a test tag was null")

    val testName = registerTest(specText, testFun)

    val oldBundle = atomic.get
    var (trunk, currentBranch, tagsMap, testsList, registrationClosed2) = oldBundle.unpack
    val tagNames = Set[String]() ++ testTags.map(_.name)
    if (!tagNames.isEmpty)
      tagsMap += (testName -> tagNames)

    updateAtomic(oldBundle, Bundle(trunk, currentBranch, tagsMap, testsList, registrationClosed2))
  }

  /**
   * Register a test with the given spec text and test function value that takes no arguments.
   *
   * This method will register the test for later execution via an invocation of one of the <code>execute</code>
   * methods. The name of the test will be a concatenation of the text of all surrounding describers,
   * from outside in, and the passed spec text, with one space placed between each item. (See the documenation
   * for <code>testNames</code> for an example.) The resulting test name must not have been registered previously on
   * this <code>Spec</code> instance.
   *
   * @param specText the specification text, which will be combined with the descText of any surrounding describers
   * to form the test name
   * @param testFun the test function
   * @throws DuplicateTestNameException if a test with the same name has been registered previously
   * @throws TestRegistrationClosedException if invoked after <code>run</code> has been invoked on this suite
   * @throws NullPointerException if <code>specText</code> or any passed test tag is <code>null</code>
   */
  /* protected def it(specText: String)(testFun: => Unit) {
    if (atomic.get.registrationClosed)
      throw new TestRegistrationClosedException(Resources("itCannotAppearInsideAnotherIt"), getStackDepth("Spec.scala", "it"))
    it(specText, Array[Tag](): _*)(testFun)
  } */

  /**
   * Register a test to ignore, which has the given spec text, optional tags, and test function value that takes no arguments.
   * This method will register the test for later ignoring via an invocation of one of the <code>execute</code>
   * methods. This method exists to make it easy to ignore an existing test by changing the call to <code>it</code>
   * to <code>ignore</code> without deleting or commenting out the actual test code. The test will not be executed, but a
   * report will be sent that indicates the test was ignored. The name of the test will be a concatenation of the text of all surrounding describers,
   * from outside in, and the passed spec text, with one space placed between each item. (See the documenation
   * for <code>testNames</code> for an example.) The resulting test name must not have been registered previously on
   * this <code>Spec</code> instance.
   *
   * @param specText the specification text, which will be combined with the descText of any surrounding describers
   * to form the test name
   * @param testTags the optional list of tags for this test
   * @param testFun the test function
   * @throws DuplicateTestNameException if a test with the same name has been registered previously
   * @throws TestRegistrationClosedException if invoked after <code>run</code> has been invoked on this suite
   * @throws NullPointerException if <code>specText</code> or any passed test tag is <code>null</code>
   */
  private def registerTestToIgnore(specText: String, testTags: List[Tag], testFun: () => Unit) {
    if (atomic.get.registrationClosed)
      throw new TestRegistrationClosedException(Resources("ignoreCannotAppearInsideAnIt"), getStackDepth("Spec.scala", "ignore"))
    if (specText == null)
      throw new NullPointerException("specText was null")
    if (testTags.exists(_ == null))
      throw new NullPointerException("a test tag was null")
    val testName = registerTest(specText, testFun)
    val tagNames = Set[String]() ++ testTags.map(_.name)
    val oldBundle = atomic.get
    var (trunk, currentBranch, tagsMap, testsList, registrationClosed) = oldBundle.unpack
    tagsMap += (testName -> (tagNames + IgnoreTagName))
    updateAtomic(oldBundle, Bundle(trunk, currentBranch, tagsMap, testsList, registrationClosed))
  }

  /**
   * Register a test to ignore, which has the given spec text and test function value that takes no arguments.
   * This method will register the test for later ignoring via an invocation of one of the <code>execute</code>
   * methods. This method exists to make it easy to ignore an existing test by changing the call to <code>it</code>
   * to <code>ignore</code> without deleting or commenting out the actual test code. The test will not be executed, but a
   * report will be sent that indicates the test was ignored. The name of the test will be a concatenation of the text of all surrounding describers,
   * from outside in, and the passed spec text, with one space placed between each item. (See the documenation
   * for <code>testNames</code> for an example.) The resulting test name must not have been registered previously on
   * this <code>Spec</code> instance.
   *
   * @param specText the specification text, which will be combined with the descText of any surrounding describers
   * to form the test name
   * @param testFun the test function
   * @throws DuplicateTestNameException if a test with the same name has been registered previously
   * @throws TestRegistrationClosedException if invoked after <code>run</code> has been invoked on this suite
   * @throws NullPointerException if <code>specText</code> or any passed test tag is <code>null</code>
   */
  /* protected def ignore(specText: String)(testFun: => Unit) {
    if (atomic.get.registrationClosed)
      throw new TestRegistrationClosedException(Resources("ignoreCannotAppearInsideAnIt"), getStackDepth("Spec.scala", "ignore"))
    ignore(specText, Array[Tag](): _*)(testFun)
  } */
  
  /**
   * Describe a &#8220;subject&#8221; being specified and tested by the passed function value. The
   * passed function value may contain more describers (defined with <code>describe</code>) and/or tests
   * (defined with <code>it</code>). This trait's implementation of this method will register the
   * description string and immediately invoke the passed function.
   */
  private def registerVerbBranch(description: String, verb: String, f: () => Unit) {
    registerBranch(
      f,
      currentBranch => {
        val desc =
          currentBranch match { // TODO, probably make a WhenDescriptionBranch so this is more type safe than looking for (when at the end
            case DescriptionBranch(parent, descriptionName) if descriptionName.endsWith(" (when") =>
              description + ")"
            case _ => description
          }
        VerbBranch(currentBranch, desc, verb)
      }
    )
  }

  private def registerDescriptionBranch(description: String, f: () => Unit) {
    registerBranch(f, DescriptionBranch(_, description))
  }

  private def registerBranch(f: () => Unit, constructBranch: Branch => Branch) {

    if (atomic.get.registrationClosed)
      throw new TestRegistrationClosedException(Resources("describeCannotAppearInsideAnIt"), getStackDepth("Spec.scala", "describe"))

    def createNewBranch() = {
      val oldBundle = atomic.get
      var (trunk, currentBranch, tagsMap, testsList, registrationClosed) = oldBundle.unpack

      val newBranch = constructBranch(currentBranch)
      // val newBranch = VerbBranch(currentBranch, description, verb)
      val oldBranch = currentBranch
      currentBranch.subNodes ::= newBranch
      currentBranch = newBranch

      updateAtomic(oldBundle, Bundle(trunk, currentBranch, tagsMap, testsList, registrationClosed))

      oldBranch
    }

    val oldBranch = createNewBranch()

    f()

    val oldBundle = atomic.get
    val (trunk, currentBranch, tagsMap, testsList, registrationClosed) = oldBundle.unpack

    updateAtomic(oldBundle, Bundle(trunk, oldBranch, tagsMap, testsList, registrationClosed))
  }

  /**
   * Class that supports the registration of tagged tests.
   *
   * <p>
   * Instances of this class are returned by the <code>taggedAs</code> method of 
   * class <code>WordSpecStringWrapper</code>.
   * </p>
   *
   * @author Bill Venners
   */
  protected final class ResultOfTaggedAsInvocationOnString(specText: String, tags: List[Tag]) {

    /**
     * Supports tagged test registration.
     *
     * <p>
     * For example, this method supports syntax such as the following:
     * </p>
     *
     * <pre>
     * "complain on peek" taggedAs(SlowTest) in { ... }
     *                                       ^
     * </pre>
     *
     * <p>
     * For more information and examples of this method's use, see the <a href="WordSpec.html">main documentation</a> for trait <code>WordSpec</code>.
     * </p>
     */
    def in(testFun: => Unit) {
      registerTestToRun(specText, tags, testFun _)
    }

    /**
     * Supports registration of tagged, pending tests.
     *
     * <p>
     * For example, this method supports syntax such as the following:
     * </p>
     *
     * <pre>
     * "complain on peek" taggedAs(SlowTest) is (pending)
     *                                       ^
     * </pre>
     *
     * <p>
     * For more information and examples of this method's use, see the <a href="WordSpec.html">main documentation</a> for trait <code>WordSpec</code>.
     * </p>
     */
    def is(testFun: => PendingNothing) {
      registerTestToRun(specText, tags, testFun _)
    }

    /**
     * Supports registration of tagged, ignored tests.
     *
     * <p>
     * For example, this method supports syntax such as the following:
     * </p>
     *
     * <pre>
     * "complain on peek" taggedAs(SlowTest) ignore { ... }
     *                                       ^
     * </pre>
     *
     * <p>
     * For more information and examples of this method's use, see the <a href="WordSpec.html">main documentation</a> for trait <code>WordSpec</code>.
     * </p>
     */
    def ignore(testFun: => Unit) {
      registerTestToIgnore(specText, tags, testFun _)
    }
  }       

  /**
   * A class that via an implicit conversion (named <code>convertToWordSpecStringWrapper</code>) enables
   * methods <code>when</code>, <code>that</code>, <code>in</code>, <code>is</code>, <code>taggedAs</code>
   * and <code>ignore</code> to be invoked on <code>String</code>s.
   *
   * <p>
   * This class provides much of the syntax for <code>WordSpec</code>, however, it does not add
   * the verb methods (<code>should</code>, <code>must</code>, and <code>can</code>) to <code>String</code>.
   * Instead, these are added via the <code>ShouldVerb</code>, <code>MustVerb</code>, and <code>CanVerb</code>
   * traits, which <code>WordSpec</code> mixes in, to avoid a conflict with implicit conversions provided
   * in <code>ShouldMatchers</code> and <code>MustMatchers</code>. 
   * </p>
   *
   * @author Bill Venners
   */
  protected final class WordSpecStringWrapper(string: String) {

    /**
     * Supports test registration.
     *
     * <p>
     * For example, this method supports syntax such as the following:
     * </p>
     *
     * <pre>
     * "complain on peek" in { ... }
     *                    ^
     * </pre>
     *
     * <p>
     * For more information and examples of this method's use, see the <a href="WordSpec.html">main documentation</a> for trait <code>WordSpec</code>.
     * </p>
     */
    def in(f: => Unit) {
      registerTestToRun(string, List(), f _)
    }

    /**
     * Supports ignored test registration.
     *
     * <p>
     * For example, this method supports syntax such as the following:
     * </p>
     *
     * <pre>
     * "complain on peek" ignore { ... }
     *                    ^
     * </pre>
     *
     * <p>
     * For more information and examples of this method's use, see the <a href="WordSpec.html">main documentation</a> for trait <code>WordSpec</code>.
     * </p>
     */
    def ignore(f: => Unit) {
      registerTestToIgnore(string, List(), f _)
    }

    /**
     * Supports pending test registration.
     *
     * <p>
     * For example, this method supports syntax such as the following:
     * </p>
     *
     * <pre>
     * "complain on peek" is (pending)
     *                    ^
     * </pre>
     *
     * <p>
     * For more information and examples of this method's use, see the <a href="WordSpec.html">main documentation</a> for trait <code>WordSpec</code>.
     * </p>
     */
    def is(f: => PendingNothing) {
      registerTestToRun(string, List(), f _)
    }

    /**
     * Supports tagged test registration.
     *
     * <p>
     * For example, this method supports syntax such as the following:
     * </p>
     *
     * <pre>
     * "complain on peek" taggedAs(SlowTest) in { ... }
     *                    ^
     * </pre>
     *
     * <p>
     * For more information and examples of this method's use, see the <a href="WordSpec.html">main documentation</a> for trait <code>WordSpec</code>.
     * </p>
     */
    def taggedAs(firstTestTag: Tag, otherTestTags: Tag*) = {
      val tagList = firstTestTag :: otherTestTags.toList
      new ResultOfTaggedAsInvocationOnString(string, tagList)
    }

    /**
     * Registers a <code>when</code> clause.
     *
     * <p>
     * For example, this method supports syntax such as the following:
     * </p>
     *
     * <pre>
     * "A Stack" when { ... }
     *           ^
     * </pre>
     *
     * <p>
     * For more information and examples of this method's use, see the <a href="WordSpec.html">main documentation</a> for trait <code>WordSpec</code>.
     * </p>
     */
    def when(f: => Unit) {
      registerDescriptionBranch(string + " (when", f _)
    }

    /**
     * Registers a <code>when</code> clause that is followed by an <em>after word</em>.
     *
     * <p>
     * For example, this method supports syntax such as the following:
     * </p>
     *
     * <pre>
     * val theUser = afterWord("the user")
     *
     * "A Stack" when theUser { ... }
     *           ^
     * </pre>
     *
     * <p>
     * For more information and examples of this method's use, see the <a href="WordSpec.html">main documentation</a> for trait <code>WordSpec</code>.
     * </p>
     */
    def when(resultOfAfterWordApplication: ResultOfAfterWordApplication) {
      registerDescriptionBranch(string + " (when " + resultOfAfterWordApplication.text, resultOfAfterWordApplication.f)
    }

    /**
     * Registers a <code>that</code> clause.
     *
     * <p>
     * For example, this method supports syntax such as the following:
     * </p>
     *
     * <pre>
     * "a rerun button" that {
     *                  ^
     * </pre>
     *
     * <p>
     * For more information and examples of this method's use, see the <a href="WordSpec.html">main documentation</a> for trait <code>WordSpec</code>.
     * </p>
     */
    def that(f: => Unit) {
      registerDescriptionBranch(string + " that", f _)
    }

    /**
     * Registers a <code>that</code> clause that is followed by an <em>after word</em>.
     *
     * <p>
     * For example, this method supports syntax such as the following:
     * </p>
     *
     * <pre>
     * def is = afterWord("is")
     *
     * "a rerun button" that is {
     *                  ^
     * </pre>
     *
     * <p>
     * For more information and examples of this method's use, see the <a href="WordSpec.html">main documentation</a> for trait <code>WordSpec</code>.
     * </p>
     */
    def that(resultOfAfterWordApplication: ResultOfAfterWordApplication) {
      registerDescriptionBranch(string + " that " + resultOfAfterWordApplication.text, resultOfAfterWordApplication.f)
    }
  }

  /**
   * Class whose instances are <em>after word</em>s, which can be used to reduce text duplication.
   *
   * <p>
   * If you are repeating a word or phrase at the beginning of each string inside
   * a block, you can "move the word or phrase" out of the block with an after word.
   * You create an after word by passing the repeated word or phrase to the <code>afterWord</code> method.
   * Once created, you can place the after word after <code>when</code>, a verb
   * (<code>should</code>, <code>must</code>, or <code>can</code>), or
   * <code>that</code>. (You can't place one after <code>in</code> or <code>is</code>, the
   * words that introduce a test.) Here's an example that has after words used in all three
   * places:
   * </p>
   *
   * <pre>
   * import org.scalatest.WordSpec
   * 
   * class ScalaTestGUISpec extends WordSpec {
   * 
   *   def theUser = afterWord("the user")
   *   def display = afterWord("display")
   *   def is = afterWord("is")
   * 
   *   "The ScalaTest GUI" when theUser {
   *     "clicks on an event report in the list box" should display {
   *       "a blue background in the clicked-on row in the list box" in {}
   *       "the details for the event in the details area" in {}
   *       "a rerun button" that is {
   *         "enabled if the clicked-on event is rerunnable" in {}
   *         "disabled if the clicked-on event is not rerunnable" in {}
   *       }
   *     }
   *   }
   * }
   * </pre>
   *
   * <p>
   * Running the previous <code>WordSpec</code> in the Scala interpreter would yield:
   * </p>
   *
   * <pre>
   * scala> (new ScalaTestGUISpec).execute()
   * The ScalaTest GUI (when the user clicks on an event report in the list box) 
   * - should display a blue background in the clicked-on row in the list box
   * - should display the details for the event in the details area
   * - should display a rerun button that is enabled if the clicked-on event is rerunnable
   * - should display a rerun button that is disabled if the clicked-on event is not rerunnable
   * </pre>
   */
  protected final class AfterWord(text: String) {

    /**
     * Supports the use of <em>after words</em>.
     *
     * <p>
     * This method transforms a block of code into a <code>ResultOfAfterWordApplication</code>, which
     * is accepted by <code>when</code>, <code>should</code>, <code>must</code>, <code>can</code>, and <code>that</code>
     * methods.  For more information, see the <a href="WordSpec.html#AfterWords">main documentation</code></a> for trait <code>WordSpec</code>.
     * </p>
     */
    def apply(f: => Unit) = new ResultOfAfterWordApplication(text, f _)
  }

  /**
   * Creates an <em>after word</em> that an be used to reduce text duplication.
   *
   * <p>
   * If you are repeating a word or phrase at the beginning of each string inside
   * a block, you can "move the word or phrase" out of the block with an after word.
   * You create an after word by passing the repeated word or phrase to the <code>afterWord</code> method.
   * Once created, you can place the after word after <code>when</code>, a verb
   * (<code>should</code>, <code>must</code>, or <code>can</code>), or
   * <code>that</code>. (You can't place one after <code>in</code> or <code>is</code>, the
   * words that introduce a test.) Here's an example that has after words used in all three
   * places:
   * </p>
   *
   * <pre>
   * import org.scalatest.WordSpec
   * 
   * class ScalaTestGUISpec extends WordSpec {
   * 
   *   def theUser = afterWord("the user")
   *   def display = afterWord("display")
   *   def is = afterWord("is")
   * 
   *   "The ScalaTest GUI" when theUser {
   *     "clicks on an event report in the list box" should display {
   *       "a blue background in the clicked-on row in the list box" in {}
   *       "the details for the event in the details area" in {}
   *       "a rerun button" that is {
   *         "enabled if the clicked-on event is rerunnable" in {}
   *         "disabled if the clicked-on event is not rerunnable" in {}
   *       }
   *     }
   *   }
   * }
   * </pre>
   *
   * <p>
   * Running the previous <code>WordSpec</code> in the Scala interpreter would yield:
   * </p>
   *
   * <pre>
   * scala> (new ScalaTestGUISpec).execute()
   * The ScalaTest GUI (when the user clicks on an event report in the list box) 
   * - should display a blue background in the clicked-on row in the list box
   * - should display the details for the event in the details area
   * - should display a rerun button that is enabled if the clicked-on event is rerunnable
   * - should display a rerun button that is disabled if the clicked-on event is not rerunnable
   * </pre>
   */
  protected def afterWord(text: String) = new AfterWord(text)

  /**
   * Implicitly converts <code>String</code>s to <code>WordSpecStringWrapper</code>, which enables
   * methods <code>when</code>, <code>that</code>, <code>in</code>, <code>is</code>, <code>taggedAs</code>
   * and <code>ignore</code> to be invoked on <code>String</code>s.
   */
  protected implicit def convertToWordSpecStringWrapper(s: String) = new WordSpecStringWrapper(s)

  // Used to enable should/can/must to take a block (except one that results in type string. May
  // want to mention this as a gotcha.)
  /*
import org.scalatest.WordSpec

class MySpec extends WordSpec {

  "bla bla bla" should {
     "do something" in {
        assert(1 + 1 === 2)
      }
      "now it is a string"
   }
}
delme.scala:6: error: no implicit argument matching parameter type (String, String, String) => org.scalatest.verb.ResultOfStringPassedToVerb was found.
  "bla bla bla" should {
                ^
one error found
  
   */
  /**
   * Supports the registration of subjects.
   *
   * <p>
   * For example, this method enables syntax such as the following:
   * </p>
   *
   * <pre>
   * "A Stack" should { ...
   *           ^
   * </pre>
   *
   * <p>
   * This function is passed as an implicit parameter to a <code>should</code> method
   * provided in <code>ShouldVerb</code>, a <code>must</code> method
   * provided in <code>MustVerb</code>, and a <code>can</code> method
   * provided in <code>CanVerb</code>. When invoked, this function registers the
   * subject and executes the block.
   * </p>
   */
  protected implicit val subjectRegistrationFunction: StringVerbBlockRegistration =
    new StringVerbBlockRegistration {
      def apply(left: String, verb: String, f: () => Unit) = registerVerbBranch(left, verb, f)
    }

  /**
   * Supports the registration of subject descriptions with after words.
   *
   * <p>
   * For example, this method enables syntax such as the following:
   * </p>
   *
   * <pre>
   * def provide = afterWord("provide")
   *
   * "The ScalaTest Matchers DSL" can provide { ... }
   *                              ^
   * </pre>
   *
   * <p>
   * This function is passed as an implicit parameter to a <code>should</code> method
   * provided in <code>ShouldVerb</code>, a <code>must</code> method
   * provided in <code>MustVerb</code>, and a <code>can</code> method
   * provided in <code>CanVerb</code>. When invoked, this function registers the
   * subject and executes the block.
   * </p>
   */
  protected implicit val subjectWithAfterWordRegistrationFunction: (String, String, ResultOfAfterWordApplication) => Unit = {
    (left, verb, resultOfAfterWordApplication) => {
      val afterWordFunction =
        () => {
          registerDescriptionBranch(resultOfAfterWordApplication.text, resultOfAfterWordApplication.f)
        }
      registerVerbBranch(left, verb, afterWordFunction)
    }
  }

  /**
   * A <code>Map</code> whose keys are <code>String</code> tag names to which tests in this <code>Spec</code> belong, and values
   * the <code>Set</code> of test names that belong to each tag. If this <code>WordSpec</code> contains no tags, this method returns an empty <code>Map</code>.
   *
   * <p>
   * This trait's implementation returns tags that were passed as strings contained in <code>Tag</code> objects passed to 
   * methods <code>test</code> and <code>ignore</code>. 
   * </p>
   */
  override def tags: Map[String, Set[String]] = atomic.get.tagsMap

  private def runTestsInBranch(branch: Branch, reporter: Reporter, stopper: Stopper, filter: Filter, configMap: Map[String, Any], tracker: Tracker) {

    val stopRequested = stopper
    // Wrap any non-DispatchReporter, non-CatchReporter in a CatchReporter,
    // so that exceptions are caught and transformed
    // into error messages on the standard error stream.
    val report = wrapReporterIfNecessary(reporter)
    branch match {
      case desc @ VerbBranch(_, descriptionName, verb) =>

        // Need to use the full name of the description, which includes all the descriptions it is nested inside
        // Call getPrefix and pass in this Desc, to get the full name
        val descriptionFullName = getPrefixWithoutVerb(desc).trim

        report(InfoProvided(tracker.nextOrdinal(), descriptionFullName, Some(NameInfo(thisSuite.suiteName, Some(thisSuite.getClass.getName), None)), None, None, Some(IndentedText(descriptionFullName, descriptionFullName, 0))))

      case _ =>
    }
    branch.subNodes.reverse.foreach(
      _ match {
        case TestLeaf(parent, tn, specText, _) =>
          if (!stopRequested()) { // TODO: Seems odd to me to check for stop here but still fire infos
            val (filterTest, ignoreTest) = filter(tn, tags)
            if (!filterTest)
              if (ignoreTest) {
                val testSucceededIcon = Resources("testSucceededIconChar")
                val formattedSpecText = Resources("iconPlusShortName", testSucceededIcon, getFormattedSpecTextPrefix(parent) + " " + specText)
                report(TestIgnored(tracker.nextOrdinal(), thisSuite.suiteName, Some(thisSuite.getClass.getName), tn, Some(IndentedText(formattedSpecText, specText, 1))))
              }
              else
                runTest(tn, report, stopRequested, configMap, tracker)
          }
        case InfoLeaf(_, message) =>
          val infoProvidedIcon = Resources("infoProvidedIconChar")
          val formattedText = Resources("iconPlusShortName", infoProvidedIcon, message)
          report(InfoProvided(tracker.nextOrdinal(), message,
            Some(NameInfo(thisSuite.suiteName, Some(thisSuite.getClass.getName), None)), None,
              None, Some(IndentedText(formattedText, message, 1))))
        case branch: Branch => runTestsInBranch(branch, reporter, stopRequested, filter, configMap, tracker)
      }
    )
  }

  /**
   * Run a test. This trait's implementation runs the test registered with the name specified by
   * <code>testName</code>. Each test's name is a concatenation of the text of all describers surrounding a test,
   * from outside in, and the test's  spec text, with one space placed between each item. (See the documenation
   * for <code>testNames</code> for an example.)
   *
   * @param testName the name of one test to execute.
   * @param reporter the <code>Reporter</code> to which results will be reported
   * @param stopper the <code>Stopper</code> that will be consulted to determine whether to stop execution early.
   * @param configMap a <code>Map</code> of properties that can be used by this <code>Spec</code>'s executing tests.
   * @throws NullPointerException if any of <code>testName</code>, <code>reporter</code>, <code>stopper</code>, or <code>configMap</code>
   *     is <code>null</code>.
   */
  protected override def runTest(testName: String, reporter: Reporter, stopper: Stopper, configMap: Map[String, Any], tracker: Tracker) {

    if (testName == null || reporter == null || stopper == null || configMap == null)
      throw new NullPointerException

    atomic.get.testsList.find(_.testName == testName) match {
      case None => throw new IllegalArgumentException("Requested test doesn't exist: " + testName)
      case Some(test) => {
        val report = wrapReporterIfNecessary(reporter)

        val testSucceededIcon = Resources("testSucceededIconChar")
        val formattedSpecText = Resources("iconPlusShortName", testSucceededIcon, getFormattedSpecTextPrefix(test.parent) + " " + test.specText)

        // Create a Rerunner if the Spec has a no-arg constructor
        val hasPublicNoArgConstructor = Suite.checkForPublicNoArgConstructor(getClass)

        val rerunnable =
          if (hasPublicNoArgConstructor)
            Some(new TestRerunner(getClass.getName, testName))
          else
            None

        val testStartTime = System.currentTimeMillis

        // A TestStarting event won't normally show up in a specification-style output, but
        // will show up in a test-style output.
        report(TestStarting(tracker.nextOrdinal(), thisSuite.suiteName, Some(thisSuite.getClass.getName), test.testName, Some(MotionToSuppress), rerunnable))

        val formatter = IndentedText(formattedSpecText, getFormattedSpecTextPrefix(test.parent) + " " + test.specText, 1)
        val informerForThisTest =
          new MessageRecordingInformer(NameInfo(thisSuite.suiteName, Some(thisSuite.getClass.getName), Some(testName))) {
            def apply(message: String) {
              if (message == null)
                throw new NullPointerException
              if (shouldRecord)
                record(message)
              else {
                val infoProvidedIcon = Resources("infoProvidedIconChar")
                val formattedText = "  " + Resources("iconPlusShortName", infoProvidedIcon, message)
                report(InfoProvided(tracker.nextOrdinal(), message, nameInfoForCurrentThread, None, None, Some(IndentedText(formattedText, message, 2))))
              }
            }
          }

        val oldInformer = atomicInformer.getAndSet(informerForThisTest)
        var testWasPending = false
        var swapAndCompareSucceeded = false
        try {
          val theConfigMap = configMap
          withFixture(
            new NoArgTest {
              def name = testName
              def apply() { test.f() }
              def configMap = theConfigMap
            }
          )

          val duration = System.currentTimeMillis - testStartTime
          report(TestSucceeded(tracker.nextOrdinal(), thisSuite.suiteName, Some(thisSuite.getClass.getName), test.testName, Some(duration), Some(formatter), rerunnable))
        }
        catch {
          case _: TestPendingException =>
            report(TestPending(tracker.nextOrdinal(), thisSuite.suiteName, Some(thisSuite.getClass.getName), test.testName, Some(formatter)))
            testWasPending = true
          case e if !anErrorThatShouldCauseAnAbort(e) =>
            val duration = System.currentTimeMillis - testStartTime
            handleFailedTest(e, false, test.testName, test, formattedSpecText, rerunnable, report, tracker, duration)
          case e => throw e
        }
        finally {
          // send out any recorded messages
          for (message <- informerForThisTest.recordedMessages) {
            val infoProvidedIcon = Resources("infoProvidedIconChar")
            val formattedText = "  " + Resources("iconPlusShortName", infoProvidedIcon, message)
            report(InfoProvided(tracker.nextOrdinal(), message, informerForThisTest.nameInfoForCurrentThread, Some(testWasPending), None, Some(IndentedText(formattedText, message, 2))))
          }

          val shouldBeInformerForThisTest = atomicInformer.getAndSet(oldInformer)
          swapAndCompareSucceeded = shouldBeInformerForThisTest eq informerForThisTest
        }
        if (!swapAndCompareSucceeded)  // Do outside finally to workaround Scala compiler bug
          throw new ConcurrentModificationException(Resources("concurrentInformerMod", thisSuite.getClass.getName))
      }
    }
  }

  private def handleFailedTest(throwable: Throwable, hasPublicNoArgConstructor: Boolean, testName: String,
      test: TestLeaf, formattedSpecText: String, rerunnable: Option[Rerunner], report: Reporter, tracker: Tracker, duration: Long) {

    val message =
      if (throwable.getMessage != null) // [bv: this could be factored out into a helper method]
        throwable.getMessage
      else
        throwable.toString

    val formatter = IndentedText(formattedSpecText, getFormattedSpecTextPrefix(test.parent) + " " + test.specText, 1)
    report(TestFailed(tracker.nextOrdinal(), message, thisSuite.suiteName, Some(thisSuite.getClass.getName), testName, Some(throwable), Some(duration), Some(formatter), rerunnable))
  }

  /**
   * Run zero to many of this <code>WordSpec</code>'s tests.
   *
   * <p>
   * This method takes a <code>testName</code> parameter that optionally specifies a test to invoke.
   * If <code>testName</code> is <code>Some</code>, this trait's implementation of this method
   * invokes <code>runTest</code> on this object, passing in:
   * </p>
   *
   * <ul>
   * <li><code>testName</code> - the <code>String</code> value of the <code>testName</code> <code>Option</code> passed
   *   to this method</li>
   * <li><code>reporter</code> - the <code>Reporter</code> passed to this method, or one that wraps and delegates to it</li>
   * <li><code>stopper</code> - the <code>Stopper</code> passed to this method, or one that wraps and delegates to it</li>
   * <li><code>configMap</code> - the <code>configMap</code> passed to this method, or one that wraps and delegates to it</li>
   * </ul>
   *
   * <p>
   * This method takes a <code>Set</code> of tag names that should be included (<code>tagsToInclude</code>), and a <code>Set</code>
   * that should be excluded (<code>tagsToExclude</code>), when deciding which of this <code>Suite</code>'s tests to execute.
   * If <code>tagsToInclude</code> is empty, all tests will be executed
   * except those those belonging to tags listed in the <code>tagsToExclude</code> <code>Set</code>. If <code>tagsToInclude</code> is non-empty, only tests
   * belonging to tags mentioned in <code>tagsToInclude</code>, and not mentioned in <code>tagsToExclude</code>
   * will be executed. However, if <code>testName</code> is <code>Some</code>, <code>tagsToInclude</code> and <code>tagsToExclude</code> are essentially ignored.
   * Only if <code>testName</code> is <code>None</code> will <code>tagsToInclude</code> and <code>tagsToExclude</code> be consulted to
   * determine which of the tests named in the <code>testNames</code> <code>Set</code> should be run. For more information on trait tags, see the main documentation for this trait.
   * </p>
   *
   * <p>
   * If <code>testName</code> is <code>None</code>, this trait's implementation of this method
   * invokes <code>testNames</code> on this <code>Suite</code> to get a <code>Set</code> of names of tests to potentially execute.
   * (A <code>testNames</code> value of <code>None</code> essentially acts as a wildcard that means all tests in
   * this <code>Suite</code> that are selected by <code>tagsToInclude</code> and <code>tagsToExclude</code> should be executed.)
   * For each test in the <code>testName</code> <code>Set</code>, in the order
   * they appear in the iterator obtained by invoking the <code>elements</code> method on the <code>Set</code>, this trait's implementation
   * of this method checks whether the test should be run based on the <code>tagsToInclude</code> and <code>tagsToExclude</code> <code>Set</code>s.
   * If so, this implementation invokes <code>runTest</code>, passing in:
   * </p>
   *
   * <ul>
   * <li><code>testName</code> - the <code>String</code> name of the test to run (which will be one of the names in the <code>testNames</code> <code>Set</code>)</li>
   * <li><code>reporter</code> - the <code>Reporter</code> passed to this method, or one that wraps and delegates to it</li>
   * <li><code>stopper</code> - the <code>Stopper</code> passed to this method, or one that wraps and delegates to it</li>
   * <li><code>configMap</code> - the <code>configMap</code> passed to this method, or one that wraps and delegates to it</li>
   * </ul>
   *
   * @param testName an optional name of one test to run. If <code>None</code>, all relevant tests should be run.
   *                 I.e., <code>None</code> acts like a wildcard that means run all relevant tests in this <code>Suite</code>.
   * @param reporter the <code>Reporter</code> to which results will be reported
   * @param stopper the <code>Stopper</code> that will be consulted to determine whether to stop execution early.
   * @param filter a <code>Filter</code> with which to filter tests based on their tags
   * @param configMap a <code>Map</code> of key-value pairs that can be used by the executing <code>Suite</code> of tests.
   * @param distributor an optional <code>Distributor</code>, into which to put nested <code>Suite</code>s to be run
   *              by another entity, such as concurrently by a pool of threads. If <code>None</code>, nested <code>Suite</code>s will be run sequentially.
   * @param tracker a <code>Tracker</code> tracking <code>Ordinal</code>s being fired by the current thread.
   * @throws NullPointerException if any of the passed parameters is <code>null</code>.
   * @throws IllegalArgumentException if <code>testName</code> is defined, but no test with the specified test name
   *     exists in this <code>Suite</code>
   */
  protected override def runTests(testName: Option[String], reporter: Reporter, stopper: Stopper, filter: Filter,
      configMap: Map[String, Any], distributor: Option[Distributor], tracker: Tracker) {
    
    if (testName == null)
      throw new NullPointerException("testName was null")
    if (reporter == null)
      throw new NullPointerException("reporter was null")
    if (stopper == null)
      throw new NullPointerException("stopper was null")
    if (filter == null)
      throw new NullPointerException("filter was null")
    if (configMap == null)
      throw new NullPointerException("configMap was null")
    if (distributor == null)
      throw new NullPointerException("distributor was null")
    if (tracker == null)
      throw new NullPointerException("tracker was null")

    val stopRequested = stopper

    testName match {
      case None => runTestsInBranch(atomic.get.trunk, reporter, stopRequested, filter, configMap, tracker)
      case Some(tn) => runTest(tn, reporter, stopRequested, configMap, tracker)
    }
  }

  /**
   * An immutable <code>Set</code> of test names. If this <code>WordSpec</code> contains no tests, this method returns an
   * empty <code>Set</code>.
   *
   * <p>
   * This trait's implementation of this method will return a set that contains the names of all registered tests. The set's
   * iterator will return those names in the order in which the tests were registered. Each test's name is composed
   * of the concatenation of the text of each surrounding describer, in order from outside in, and the text of the
   * example itself, with all components separated by a space. For example, consider this <code>WordSpec</code>:
   * </p>
   *
   * <pre>
   * import org.scalatest.WordSpec
   *
   * class StackSpec {
   *   "A Stack" when {
   *     "not empty" must {
   *       "allow me to pop" in {}
   *     }
   *     "not full" must {
   *       "allow me to push" in {}
   *     }
   *   }
   * }
   * </pre>
   *
   * <p>
   * Invoking <code>testNames</code> on this <code>Spec</code> will yield a set that contains the following
   * two test name strings:
   * </p>
   *
   * <pre>
   * "A Stack (when not empty) must allow me to pop"
   * "A Stack (when not full) must allow me to push"
   * </pre>
   */
  override def testNames: Set[String] = ListSet(atomic.get.testsList.map(_.testName): _*)

  override def run(testName: Option[String], reporter: Reporter, stopper: Stopper, filter: Filter,
      configMap: Map[String, Any], distributor: Option[Distributor], tracker: Tracker) {

    val stopRequested = stopper

    // Set the flag that indicates registration is closed (because run has now been invoked),
    // which will disallow any further invocations of "describe", it", or "ignore" with
    // an RegistrationClosedException.
    val oldBundle = atomic.get
    var (trunk, currentBranch, tagsMap, testsList, registrationClosed) = oldBundle.unpack
    if (!registrationClosed)
      updateAtomic(oldBundle, Bundle(trunk, currentBranch, tagsMap, testsList, true))

    val report = wrapReporterIfNecessary(reporter)

    val informerForThisSuite =
      new ConcurrentInformer(NameInfo(thisSuite.suiteName, Some(thisSuite.getClass.getName), None)) {
        def apply(message: String) {
          if (message == null)
            throw new NullPointerException
          report(InfoProvided(tracker.nextOrdinal(), message, nameInfoForCurrentThread))
        }
      }

    atomicInformer.set(informerForThisSuite)

    var swapAndCompareSucceeded = false
    try {
      super.run(testName, report, stopRequested, filter, configMap, distributor, tracker)
    }
    finally {
      val shouldBeInformerForThisSuite = atomicInformer.getAndSet(zombieInformer)
      swapAndCompareSucceeded = shouldBeInformerForThisSuite eq informerForThisSuite
    }
    if (!swapAndCompareSucceeded)  // Do outside finally to workaround Scala compiler bug
      throw new ConcurrentModificationException(Resources("concurrentInformerMod", thisSuite.getClass.getName))
  }

  /**
   * Supports shared test registration in <code>WordSpec</code>s.
   *
   * <p>
   * This field enables syntax such as the following:
   * </p>
   *
   * <pre>
   * behave like nonFullStack(stackWithOneItem)
   * ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of <cod>behave</code>, see the <a href="#SharedTests">Shared tests section</a>
   * in the main documentation for this trait.
   * </p>
   */
  protected val behave = new BehaveWord
}
