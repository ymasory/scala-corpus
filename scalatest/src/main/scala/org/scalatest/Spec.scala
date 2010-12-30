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

import NodeFamily._
import scala.collection.immutable.ListSet
import org.scalatest.StackDepthExceptionHelper.getStackDepth
import java.util.concurrent.atomic.AtomicReference
import java.util.ConcurrentModificationException
import org.scalatest.events._
import Suite.anErrorThatShouldCauseAnAbort
import verb.BehaveWord

/**
 * Trait that facilitates a &#8220;behavior-driven&#8221; style of development (BDD), in which tests
 * are combined with text that specifies the behavior the tests verify.
 * (Note: In BDD, the word <em>example</em> is usually used instead of <em>test</em>. The word test will not appear
 * in your code if you use <code>WordSpec</code>, so if you prefer the word <em>example</em> you can use it. However, in this documentation
 * the word <em>test</em> will be used, for clarity and to be consistent with the rest of ScalaTest.)
 * Here's an example <code>Spec</code>:
 *
 * <pre>
 * import org.scalatest.Spec
 * import scala.collection.mutable.Stack
 *
 * class StackSpec extends Spec {
 *
 *   describe("A Stack") {
 *
 *     it("should pop values in last-in-first-out order") {
 *       val stack = new Stack[Int]
 *       stack.push(1)
 *       stack.push(2)
 *       assert(stack.pop() === 2)
 *       assert(stack.pop() === 1)
 *     }
 *
 *     it("should throw NoSuchElementException if an empty stack is popped") {
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
 * A <code>Spec</code> contains <em>describe clauses</em> and tests. You define a describe clause
 * with <code>describe</code>, and a test with <code>it</code>. Both
 * <code>describe</code> and <code>it</code> are methods, defined in
 * <code>Spec</code>, which will be invoked
 * by the primary constructor of <code>StackSpec</code>. 
 * A describe clause names, or gives more information about, the <em>subject</em> (class or other entity) you are specifying
 * and testing. In the previous example, "A Stack"
 * is the subject under specification and test. With each test you provide a string (the <em>spec text</em>) that specifies
 * one bit of behavior of the subject, and a block of code that tests that behavior.
 * You place the spec text between the parentheses, followed by the test code between curly
 * braces.  The test code will be wrapped up as a function passed as a by-name parameter to
 * <code>it</code>, which will register the test for later execution.
 * </p>
 *
 * <p>
 * A <code>Spec</code>'s lifecycle has two phases: the <em>registration</em> phase and the
 * <em>ready</em> phase. It starts in registration phase and enters ready phase the first time
 * <code>run</code> is called on it. It then remains in ready phase for the remainder of its lifetime.
 * </p>
 *
 * <p>
 * Tests can only be registered with the <code>it</code> method while the <code>Spec</code> is
 * in its registration phase. Any attempt to register a test after the <code>Spec</code> has
 * entered its ready phase, <em>i.e.</em>, after <code>run</code> has been invoked on the <code>Spec</code>,
 * will be met with a thrown <code>TestRegistrationClosedException</code>. The recommended style
 * of using <code>Spec</code> is to register tests during object construction as is done in all
 * the examples shown here. If you keep to the recommended style, you should never see a
 * <code>TestRegistrationClosedException</code>.
 * </p>
 *
 * <p>
 * When you execute a <code>Spec</code>, it will send <code>Formatter</code>s in the events it sends to the
 * <code>Reporter</code>. ScalaTest's built-in reporters will report these events in such a way
 * that the output is easy to read as an informal specification of the <em>subject</em> being tested.
 * For example, if you ran <code>StackSpec</code> from within the Scala interpreter:
 * </p>
 *
 * <pre>
 * scala> (new StackSpec).execute()
 * </pre>
 *
 * <p>
 * You would see:
 * </p>
 *
 * <pre>
 * A Stack
 * - should pop values in last-in-first-out order
 * - should throw NoSuchElementException if an empty stack is popped
 * </pre>
 *
 * <p>
 * <strong>Shared fixtures</strong>
 * </p>
 *
 * <p>
 * A test <em>fixture</em> is objects or other artifacts (such as files, sockets, database
 * connections, etc.) used by tests to do their work. You can use fixtures in
 * <code>Spec</code>s with the same approaches suggested for <code>Suite</code> in
 * its documentation. The same text that appears in the test fixture
 * section of <code>Suite</code>'s documentation is repeated here, with examples changed from
 * <code>Suite</code> to <code>Spec</code>.
 * </p>
 *
 * <p>
 * If a fixture is used by only one test, then the definitions of the fixture objects can
 * be local to the test function, such as the objects assigned to <code>stack</code> and <code>emptyStack</code> in the
 * previous <code>StackSpec</code> examples. If multiple tests need to share a fixture, the best approach
 * is to assign them to instance variables. Here's a (very contrived) example, in which the object assigned
 * to <code>shared</code> is used by multiple test functions:
 * </p>
 *
 * <pre>
 * import org.scalatest.Spec
 *
 * class ArithmeticSpec extends Spec {
 *
 *   // Sharing immutable fixture objects via instance variables
 *   val shared = 5
 *
 *   it("should add correctly") {
 *     val sum = 2 + 3
 *     assert(sum === shared)
 *   }
 *
 *   it("should subtract correctly") {
 *     val diff = 7 - 2
 *     assert(diff === shared)
 *   }
 * }
 * </pre>
 *
 * <p>
 * In some cases, however, shared <em>mutable</em> fixture objects may be changed by tests such that
 * they need to be recreated or reinitialized before each test. Shared resources such
 * as files or database connections may also need to 
 * be created and initialized before, and cleaned up after, each test. JUnit offers methods <code>setUp</code> and
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
 * import org.scalatest.Spec
 * import scala.collection.mutable.ListBuffer
 *
 * class MySpec extends Spec {
 *
 *   // create objects needed by tests and return as a tuple
 *   def createFixture = (
 *     new StringBuilder("ScalaTest is "),
 *     new ListBuffer[String]
 *   )
 *
 *   it("should mutate shared fixture objects") {
 *     val (builder, lbuf) = createFixture
 *     builder.append("easy!")
 *     assert(builder.toString === "ScalaTest is easy!")
 *     assert(lbuf.isEmpty)
 *     lbuf += "sweet"
 *   }
 *
 *   it("should get a fresh set of mutable fixture objects") {
 *     val (builder, lbuf) = createFixture
 *     builder.append("fun!")
 *     assert(builder.toString === "ScalaTest is fun!")
 *     assert(lbuf.isEmpty)
 *   }
 * }
 * </pre>
 *
 * <p>
 * If different tests in the same <code>Spec</code> require different fixtures, you can create multiple create-fixture methods and
 * call the method (or methods) needed by each test at the begining of the test. If every test requires the same set of
 * mutable fixture objects, one other approach you can take is make them simply <code>val</code>s and mix in trait
 * <a href="OneInstancePerTest.html"><code>OneInstancePerTest</code></a>.  If you mix in <code>OneInstancePerTest</code>, each test
 * will be run in its own instance of the <code>Spec</code>, similar to the way JUnit tests are executed.
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
 * import org.scalatest.Spec
 * import org.scalatest.BeforeAndAfterEach
 * import java.io.FileReader
 * import java.io.FileWriter
 * import java.io.File
 *
 * class MySpec extends Spec with BeforeAndAfterEach {
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
 *   it("should read from a temp file") {
 *     var builder = new StringBuilder
 *     var c = reader.read()
 *     while (c != -1) {
 *       builder.append(c.toChar)
 *       c = reader.read()
 *     }
 *     assert(builder.toString === "Hello, test!")
 *   }
 *
 *   it("should read the first char of a temp file") {
 *     assert(reader.read() === 'H')
 *   }
 * 
 *   it("should work without a fixture") {
 *     assert(1 + 1 === 2)
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
 * import org.scalatest.Spec
 * import org.scalatest.BeforeAndAfterEach
 * import java.io.FileReader
 * import java.io.FileWriter
 * import java.io.File
 *
 * class MySpec extends Spec {
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
 *   it("should read from a temp file") {
 *     var builder = new StringBuilder
 *     var c = reader.read()
 *     while (c != -1) {
 *       builder.append(c.toChar)
 *       c = reader.read()
 *     }
 *     assert(builder.toString === "Hello, test!")
 *   }
 *
 *   it("should read the first char of a temp file") {
 *     assert(reader.read() === 'H')
 *   }
 * 
 *   it("should work without a fixture") {
 *     assert(1 + 1 === 2)
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you prefer to keep your test classes immutable, one final variation is to use the
 * <a href="fixture/FixtureSpec.html"><code>FixtureSpec</code></a> trait from the
 * <code>org.scalatest.fixture</code> package.  Tests in an <code>org.scalatest.fixture.FixtureSpec</code> can have a fixture
 * object passed in as a parameter. You must indicate the type of the fixture object
 * by defining the <code>Fixture</code> type member and define a <code>withFixture</code> method that takes a <em>one-arg</em> test function.
 * (A <code>FixtureSpec</code> has two overloaded <code>withFixture</code> methods, therefore, one that takes a <code>OneArgTest</code>
 * and the other, inherited from <code>Suite</code>, that takes a <code>NoArgTest</code>.)
 * Inside the <code>withFixture(OneArgTest)</code> method, you create the fixture, pass it into the test function, then perform any
 * necessary cleanup after the test function returns. Instead of invoking each test directly, a <code>FixtureSpec</code> will
 * pass a function that invokes the code of a test to <code>withFixture(OneArgTest)</code>. Your <code>withFixture(OneArgTest)</code> method, therefore,
 * is responsible for actually running the code of the test by invoking the test function.
 * For example, you could pass the temp file reader fixture to each test that needs it
 * by overriding the <code>withFixture(OneArgTest)</code> method of a <code>FixtureSpec</code>, like this:
 * </p>
 *
 * <pre>
 * import org.scalatest.fixture.FixtureSpec
 * import java.io.FileReader
 * import java.io.FileWriter
 * import java.io.File
 * 
 * class MySuite extends FixtureSpec {
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
 *   it("should read from a temp file") { reader =>
 *     var builder = new StringBuilder
 *     var c = reader.read()
 *     while (c != -1) {
 *       builder.append(c.toChar)
 *       c = reader.read()
 *     }
 *     assert(builder.toString === "Hello, test!")
 *   }
 * 
 *   it("should read the first char of a temp file") { reader =>
 *     assert(reader.read() === 'H')
 *   }
 * 
 *   it("should work without a fixture") { () =>
 *     assert(1 + 1 === 2)
 *   }
 * }
 * </pre>
 *
 * <p>
 * It is worth noting that the only difference in the test code between the mutable
 * <code>BeforeAndAfterEach</code> approach shown here and the immutable <code>FixtureSpec</code>
 * approach shown previously is that two of the <code>FixtureSpec</code>'s test functions take a <code>FileReader</code> as
 * a parameter via the "<code>reader =></code>" at the beginning of the function. Otherwise the test code is identical.
 * One benefit of the explicit parameter is that, as demonstrated
 * by the "<code>should work without a fixture</code>" test, a <code>FixtureSpec</code>
 * test need not take the fixture. So you can have some tests that take a fixture, and others that don't.
 * In this case, the <code>FixtureSpec</code> provides documentation indicating which
 * tests use the fixture and which don't, whereas the <code>BeforeAndAfterEach</code> approach does not.
 * (If you have want to combine tests that take different fixture types in the same <code>Spec</code>, you can
 * use <a href="fixture/MultipleFixtureSpec.html">MultipleFixtureSpec</a>.)
 * </p>
 *
 * <p>
 * If you want to execute code before and after all tests (and nested suites) in a suite, such
 * want to execute code before and after all tests (and nested suites) in a suite, such
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
 * by different fixture objects.
 * To accomplish this in a <code>Spec</code>, you first place shared tests in <em>behavior functions</em>. These behavior functions will be
 * invoked during the construction phase of any <code>Spec</code> that uses them, so that the tests they contain will be registered as tests in that <code>Spec</code>.
 * For example, given this stack class:
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
 * stack fixture to use when running the tests. So in your <code>Spec</code> for stack, you'd invoke the
 * behavior function three times, passing in each of the three stack fixtures so that the shared tests are run for all three fixtures. You
 * can define a behavior function that encapsulates these shared tests inside the <code>Spec</code> that uses them. If they are shared
 * between different <code>Spec</code>s, however, you could also define them in a separate trait that is mixed into each <code>Spec</code> that uses them.
 * </p>
 *
 * <p>
 * <a name="StackBehaviors">For</a> example, here the <code>nonEmptyStack</code> behavior function (in this case, a behavior <em>method</em>) is defined in a trait along with another
 * method containing shared tests for non-full stacks:
 * </p>
 * 
 * <pre>
 * trait StackBehaviors { this: Spec =>
 * 
 *   def nonEmptyStack(stack: Stack[Int], lastItemAdded: Int) {
 * 
 *     it("should be non-empty") {
 *       assert(!stack.empty)
 *     }  
 * 
 *     it("should return the top item on peek") {
 *       assert(stack.peek === lastItemAdded)
 *     }
 *   
 *     it("should not remove the top item on peek") {
 *       val size = stack.size
 *       assert(stack.peek === lastItemAdded)
 *       assert(stack.size === size)
 *     }
 *   
 *     it("should remove the top item on pop") {
 *       val size = stack.size
 *       assert(stack.pop === lastItemAdded)
 *       assert(stack.size === size - 1)
 *     }
 *   }
 *   
 *   def nonFullStack(stack: Stack[Int]) {
 *       
 *     it("should not be full") {
 *       assert(!stack.full)
 *     }
 *       
 *     it("should add to the top on push") {
 *       val size = stack.size
 *       stack.push(7)
 *       assert(stack.size === size + 1)
 *       assert(stack.peek === 7)
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * Given these behavior functions, you could invoke them directly, but <code>Spec</code> offers a DSL for the purpose,
 * which looks like this:
 * </p>
 *
 * <pre>
 * it should behave like nonEmptyStack(stackWithOneItem, lastValuePushed)
 * it should behave like nonFullStack(stackWithOneItem)
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
 * it should behave like nonEmptyStack // assuming lastValuePushed is also in scope inside nonEmptyStack
 * it should behave like nonFullStack
 * </pre>
 *
 * <p>
 * The recommended style, however, is the functional, pass-all-the-needed-values-in style. Here's an example:
 * </p>
 *
 * <pre>
 * class SharedTestExampleSpec extends Spec with StackBehaviors {
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
 *   describe("A Stack") {
 * 
 *     describe("(when empty)") {
 *       
 *       it("should be empty") {
 *         assert(emptyStack.empty)
 *       }
 * 
 *       it("should complain on peek") {
 *         intercept[IllegalStateException] {
 *           emptyStack.peek
 *         }
 *       }
 * 
 *       it("should complain on pop") {
 *         intercept[IllegalStateException] {
 *           emptyStack.pop
 *         }
 *       }
 *     }
 * 
 *     describe("(with one item)") {
 *       it should behave like nonEmptyStack(stackWithOneItem, lastValuePushed)
 *       it should behave like nonFullStack(stackWithOneItem)
 *     }
 *     
 *     describe("(with one item less than capacity)") {
 *       it should behave like nonEmptyStack(stackWithOneItemLessThanCapacity, lastValuePushed)
 *       it should behave like nonFullStack(stackWithOneItemLessThanCapacity)
 *     }
 * 
 *     describe("(full)") {
 *       
 *       it("should be full") {
 *         assert(fullStack.full)
 *       }
 * 
 *       it should behave like nonEmptyStack(fullStack, lastValuePushed)
 * 
 *       it("should complain on a push") {
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
 * scala> (new StackSpec).execute()
 * A Stack (when empty) 
 * - should be empty
 * - should complain on peek
 * - should complain on pop
 * A Stack (with one item) 
 * - should be non-empty
 * - should return the top item on peek
 * - should not remove the top item on peek
 * - should remove the top item on pop
 * - should not be full
 * - should add to the top on push
 * A Stack (with one item less than capacity) 
 * - should be non-empty
 * - should return the top item on peek
 * - should not remove the top item on peek
 * - should remove the top item on pop
 * - should not be full
 * - should add to the top on push
 * A Stack (full) 
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
 * complaining that multiple tests are being registered with the same test name. A good way to solve this problem in a <code>Spec</code> is to surround
 * each invocation of a behavior function with a <code>describe</code> clause, which will prepend a string to each test name.
 * For example, the following code in a <code>Spec</code> would register a test with the name <code>"A Stack (when empty) should be empty"</code>:
 * </p>
 *
 * <pre>
 *   describe("A Stack") {
 * 
 *     describe("(when empty)") {
 *       
 *       it("should be empty") {
 *         assert(emptyStack.empty)
 *       }
 *       // ...
 * </pre>
 *
 * <p>
 * If the <code>"should be empty"</code> test was factored out into a behavior function, it could be called repeatedly so long
 * as each invocation of the behavior function is inside a different set of <code>describe</code> clauses.
 *
 * <p>
 * <strong>Tagging tests</strong>
 * </p>
 *
 * <p>
 * A <code>Spec</code>'s tests may be classified into groups by <em>tagging</em> them with string names.
 * As with any suite, when executing a <code>Spec</code>, groups of tests can
 * optionally be included and/or excluded. To tag a <code>Spec</code>'s tests,
 * you pass objects that extend abstract class <code>org.scalatest.Tag</code> to the methods
 * that register tests, <code>it</code> and <code>ignore</code>. Class <code>Tag</code> takes one parameter,
 * a string name.  If you have
 * created Java annotation interfaces for use as group names in direct subclasses of <code>org.scalatest.Suite</code>,
 * then you will probably want to use group names on your <code>Spec</code>s that match. To do so, simply 
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
 * Given these definitions, you could place <code>Spec</code> tests into groups like this:
 * </p>
 *
 * <pre>
 * import org.scalatest.Spec
 *
 * class MySuite extends Spec {
 *
 *   it("should add correctly", SlowTest) {
 *     val sum = 1 + 1
 *     assert(sum === 2)
 *     assert(sum + 2 === 4)
 *   }
 *
 *   it("should subtract correctly", SlowTest, DbTest) {
 *     val diff = 4 - 1
 *     assert(diff === 3)
 *     assert(diff - 2 === 1)
 *   }
 * }
 * </pre>
 *
 * <p>
 * This code marks both tests with the <code>com.mycompany.groups.SlowTest</code> tag, 
 * and test <code>"should subtract correctly"</code> with the <code>com.mycompany.groups.DbTest</code> tag.
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
 * <p>
 * To support the common use case of &#8220;temporarily&#8221; disabling a test, with the
 * good intention of resurrecting the test at a later time, <code>Spec</code> provides registration
 * methods that start with <code>ignore</code> instead of <code>it</code>. For example, to temporarily
 * disable the test with the name <code>"should pop values in last-in-first-out order"</code>, just change &#8220;<code>it</code>&#8221; into &#8220;<code>ignore</code>,&#8221; like this:
 * </p>
 *
 * <pre>
 * import org.scalatest.Spec
 * import scala.collection.mutable.Stack
 *
 * class StackSpec extends Spec {
 *
 *   describe("A Stack") {
 *
 *     ignore("should pop values in last-in-first-out order") {
 *       val stack = new Stack[Int]
 *       stack.push(1)
 *       stack.push(2)
 *       assert(stack.pop() === 2)
 *       assert(stack.pop() === 1)
 *     }
 *
 *     it("should throw NoSuchElementException if an empty stack is popped") {
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
 * the actual test, and possibly the functionality, has not yet been implemented.
 * </p>
 *
 * <p>
 * You can mark a test as pending in <code>Spec</code> by placing "<code>(pending)</code>" after the 
 * test name, like this:
 * </p>
 *
 * <pre>
 * import org.scalatest.Spec
 * import scala.collection.mutable.Stack
 *
 * class StackSpec extends Spec {
 *
 *   describe("A Stack") {
 *
 *     it("should pop values in last-in-first-out order") {
 *       val stack = new Stack[Int]
 *       stack.push(1)
 *       stack.push(2)
 *       assert(stack.pop() === 2)
 *       assert(stack.pop() === 1)
 *     }
 *
 *     it("should throw NoSuchElementException if an empty stack is popped") (pending)
 *   }
 * }
 * </pre>
 *
 * <p>
 * (Note: "<code>(pending)</code>" is the body of the test. Thus the test contains just one statement, an invocation
 * of the <code>pending</code> method, which throws <code>TestPendingException</code>.)
 * If you run this version of <code>StackSpec</code> with:
 * </p>
 *
 * <pre>
 * scala> (new StackSpec).execute()
 * </pre>
 *
 * <p>
 * It will run both tests, but report that the test named "<code>A stack should pop values in last-in-first-out order</code>" is pending. You'll see:
 * </p>
 *
 * <pre>
 * A Stack 
 * - should pop values in last-in-first-out order
 * - should throw NoSuchElementException if an empty stack is popped (pending)
 * </pre>
 * 
 * @author Bill Venners
 */
trait Spec extends Suite { thisSuite =>

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
      throw new ConcurrentModificationException(Resources("concurrentSpecBundleMod"))
  }

  private def registerTest(specText: String, f: => Unit) = {

    val oldBundle = atomic.get
    var (trunk, currentBranch, tagsMap, testsList, registrationClosed) = oldBundle.unpack

    val testName = getTestName(specText, currentBranch)
    if (testsList.exists(_.testName == testName)) {
      throw new DuplicateTestNameException(testName, getStackDepth("Spec.scala", "it"))
    }
    val testShortName = specText
    val test = TestLeaf(currentBranch, testName, specText, f _)
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
  // is the registration phase of a Spec's lifecycle.)
  private final val atomicInformer = new AtomicReference[Informer](new RegistrationInformer)

  /**
   * Returns an <code>Informer</code> that during test execution will forward strings (and other objects) passed to its
   * <code>apply</code> method to the current reporter. If invoked in a constructor, it
   * will register the passed string for forwarding later during test execution. If invoked while this
   * <code>Spec</code> is being executed, such as from inside a test function, it will forward the information to
   * the current reporter immediately. If invoked at any other time, it will
   * throw an exception. This method can be called safely by any thread.
   */
  implicit protected def info: Informer = atomicInformer.get

  private val zombieInformer =
    new Informer {
      private val complaint = Resources("cantCallInfoNow", "Spec")
      def apply(message: String) {
        if (message == null)
          throw new NullPointerException
        throw new IllegalStateException(complaint)
      }
    }

  /**
   * Class that, via an instance referenced from the <code>it</code> field,
   * supports test (and shared test) registration in <code>Spec</code>s.
   *
   * <p>
   * This class supports syntax such as the following test registration:
   * </p>
   *
   * <pre>
   * it("should be empty")
   * ^
   * </pre>
   *
   * <p>
   * and the following shared test registration:
   * </p>
   *
   * <pre>
   * it should behave like nonFullStack(stackWithOneItem)
   * ^
   * </pre>
   *
   * <p>
   * For more information and examples, see the <a href="Spec.html">main documentation for <code>Spec</code></a>.
   * </p>
   */
  protected class ItWord {

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
    def apply(specText: String, testTags: Tag*)(testFun: => Unit) {

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
    def apply(specText: String)(testFun: => Unit) {
      if (atomic.get.registrationClosed)
        throw new TestRegistrationClosedException(Resources("itCannotAppearInsideAnotherIt"), getStackDepth("Spec.scala", "it"))
      apply(specText, Array[Tag](): _*)(testFun)
    }

    /**
     * Supports the registration of shared tests.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre>
     * it should behave like nonFullStack(stackWithOneItem)
     *    ^
     * </pre>
     *
     * <p>
     * For examples of shared tests, see the <a href="Spec.html#SharedTests">Shared tests section</a>
     * in the main documentation for trait <code>Spec</code>.
     * </p>
     */
    def should(behaveWord: BehaveWord) = behaveWord

    /**
     * Supports the registration of shared tests.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre>
     * it must behave like nonFullStack(stackWithOneItem)
     *    ^
     * </pre>
     *
     * <p>
     * For examples of shared tests, see the <a href="Spec.html#SharedTests">Shared tests section</a>
     * in the main documentation for trait <code>Spec</code>.
     * </p>
     */
    def must(behaveWord: BehaveWord) = behaveWord
  }

  /**
   * Supports test (and shared test) registration in <code>Spec</code>s.
   *
   * <p>
   * This field supports syntax such as the following:
   * </p>
   *
   * <pre>
   * it("should be empty")
   * ^
   * </pre>
   *
   * <pre>
   * it should behave like nonFullStack(stackWithOneItem)
   * ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of the <code>it</code> field, see the main documentation for this trait.
   * </p>
   */
  protected val it = new ItWord

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
  protected def ignore(specText: String, testTags: Tag*)(testFun: => Unit) {
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
  protected def ignore(specText: String)(testFun: => Unit) {
    if (atomic.get.registrationClosed)
      throw new TestRegistrationClosedException(Resources("ignoreCannotAppearInsideAnIt"), getStackDepth("Spec.scala", "ignore"))
    ignore(specText, Array[Tag](): _*)(testFun)
  }
  
  /**
   * Describe a &#8220;subject&#8221; being specified and tested by the passed function value. The
   * passed function value may contain more describers (defined with <code>describe</code>) and/or tests
   * (defined with <code>it</code>). This trait's implementation of this method will register the
   * description string and immediately invoke the passed function.
   */
  protected def describe(description: String)(f: => Unit) {

    if (atomic.get.registrationClosed)
      throw new TestRegistrationClosedException(Resources("describeCannotAppearInsideAnIt"), getStackDepth("Spec.scala", "describe"))

    def createNewBranch() = {
      val oldBundle = atomic.get
      var (trunk, currentBranch, tagsMap, testsList, registrationClosed) = oldBundle.unpack

      val newBranch = DescriptionBranch(currentBranch, description)
      val oldBranch = currentBranch
      currentBranch.subNodes ::= newBranch
      currentBranch = newBranch

      updateAtomic(oldBundle, Bundle(trunk, currentBranch, tagsMap, testsList, registrationClosed))

      oldBranch
    }

    val oldBranch = createNewBranch()

    f

    val oldBundle = atomic.get
    val (trunk, currentBranch, tagsMap, testsList, registrationClosed) = oldBundle.unpack

    updateAtomic(oldBundle, Bundle(trunk, oldBranch, tagsMap, testsList, registrationClosed))
  }

  /**
   * A <code>Map</code> whose keys are <code>String</code> tag names to which tests in this <code>Spec</code> belong, and values
   * the <code>Set</code> of test names that belong to each tag. If this <code>Spec</code> contains no tags, this method returns an empty <code>Map</code>.
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
      case desc @ DescriptionBranch(_, descriptionName) =>

        def sendInfoProvidedMessage() {
          // Need to use the full name of the description, which includes all the descriptions it is nested inside
          // Call getPrefix and pass in this Desc, to get the full name
          val descriptionFullName = getPrefix(desc).trim
         

          report(InfoProvided(tracker.nextOrdinal(), descriptionFullName, Some(NameInfo(thisSuite.suiteName, Some(thisSuite.getClass.getName), None)), None, None, Some(IndentedText(descriptionFullName, descriptionFullName, 0))))
        }
        
        // Only send an infoProvided message if the first thing in the subNodes is *not* sub-description, i.e.,
        // it is a test, because otherwise we get a lame description that doesn't have any tests under it.
        // But send it if the list is empty.
        if (desc.subNodes.isEmpty)
          sendInfoProvidedMessage() 
        else
          desc.subNodes.reverse.head match {
            case ex: TestLeaf => sendInfoProvidedMessage()
            case _ => // Do nothing in this case
          }

      case _ =>
    }
    branch.subNodes.reverse.foreach(
      _ match {
        case TestLeaf(_, tn, specText, _) =>
          if (!stopRequested()) { // TODO: Seems odd to me to check for stop here but still fire infos
            val (filterTest, ignoreTest) = filter(tn, tags)
            if (!filterTest)
              if (ignoreTest) {
                val testSucceededIcon = Resources("testSucceededIconChar")
                val formattedSpecText = Resources("iconPlusShortName", testSucceededIcon, specText)
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
        val formattedSpecText = Resources("iconPlusShortName", testSucceededIcon, test.specText)

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

        val formatter = IndentedText(formattedSpecText, test.specText, 1)
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
            handleFailedTest(e, false, test.testName, test.specText, formattedSpecText, rerunnable, report, tracker, duration)
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
          if (!swapAndCompareSucceeded) {
            if (shouldBeInformerForThisTest == null) println("shouldBeInformerForThisTest was null")
            else if (shouldBeInformerForThisTest eq zombieInformer) println("shouldBeInformerForThisTest was the zombie")
            else println("shouldBeInformerForThisTest's class was: " + shouldBeInformerForThisTest.getClass.getName)
          }
        }
        if (!swapAndCompareSucceeded)  // Do outside finally to workaround Scala compiler bug
          throw new ConcurrentModificationException(Resources("concurrentInformerMod", thisSuite.getClass.getName))
      }
    }
  }

  private def handleFailedTest(throwable: Throwable, hasPublicNoArgConstructor: Boolean, testName: String,
      specText: String, formattedSpecText: String, rerunnable: Option[Rerunner], report: Reporter, tracker: Tracker, duration: Long) {

    val message =
      if (throwable.getMessage != null) // [bv: this could be factored out into a helper method]
        throwable.getMessage
      else
        throwable.toString

    val formatter = IndentedText(formattedSpecText, specText, 1)
    report(TestFailed(tracker.nextOrdinal(), message, thisSuite.suiteName, Some(thisSuite.getClass.getName), testName, Some(throwable), Some(duration), Some(formatter), rerunnable))
  }

  /**
   * Run zero to many of this <code>Spec</code>'s tests.
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
   * An immutable <code>Set</code> of test names. If this <code>Spec</code> contains no tests, this method returns an
   * empty <code>Set</code>.
   *
   * <p>
   * This trait's implementation of this method will return a set that contains the names of all registered tests. The set's
   * iterator will return those names in the order in which the tests were registered. Each test's name is composed
   * of the concatenation of the text of each surrounding describer, in order from outside in, and the text of the
   * example itself, with all components separated by a space. For example, consider this <code>Spec</code>:
   * </p>
   *
   * <pre>
   * import org.scalatest.Spec
   *
   * class StackSpec extends Spec{
   *   describe("A Stack") {
   *     describe("(when not empty)") {
   *       it("must allow me to pop") {}
   *     }
   *     describe("(when not full)") {
   *       it("must allow me to push") {}
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

  @volatile private var wasRunBefore = false
  override def run(testName: Option[String], reporter: Reporter, stopper: Stopper, filter: Filter,
      configMap: Map[String, Any], distributor: Option[Distributor], tracker: Tracker) {

    if (wasRunBefore)
      println(thisSuite.getClass.getName + ", a Spec, is being run again")
    else
      wasRunBefore = true
    
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
      if (!swapAndCompareSucceeded) {
        if (shouldBeInformerForThisSuite == null) println("shouldBeInformerForThisSuite was null")
        else if (shouldBeInformerForThisSuite eq zombieInformer) println("shouldBeInformerForThisSuite was the zombie")
        else println("shouldBeInformerForThisSuite's class was: " + shouldBeInformerForThisSuite.getClass.getName)
      }
    }
    if (!swapAndCompareSucceeded) // Do outside finally to workaround Scala compiler bug
      throw new ConcurrentModificationException(Resources("concurrentInformerMod", thisSuite.getClass.getName))
  }

  /**
   * Supports shared test registration in <code>Spec</code>s.
   *
   * <p>
   * This field supports syntax such as the following:
   * </p>
   *
   * <pre>
   * it should behave like nonFullStack(stackWithOneItem)
   *           ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of <cod>behave</code>, see the <a href="#SharedTests">Shared tests section</a>
   * in the main documentation for this trait.
   * </p>
   */
  protected val behave = new BehaveWord
}
