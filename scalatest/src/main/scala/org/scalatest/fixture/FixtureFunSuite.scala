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
package org.scalatest.fixture

import org.scalatest._
import scala.collection.immutable.ListSet
import java.util.ConcurrentModificationException
import java.util.concurrent.atomic.AtomicReference
import org.scalatest.StackDepthExceptionHelper.getStackDepth
import org.scalatest.events._
import Suite.anErrorThatShouldCauseAnAbort

/**
 * A sister trait to <code>org.scalatest.FunSuite</code> that can pass a fixture object into its tests.
 *
 * <p>
 * This trait behaves similarly to trait <code>org.scalatest.FunSuite</code>, except that tests may take a fixture object. The type of the
 * fixture object passed is defined by the abstract <code>Fixture</code> type, which is declared as a member of this trait (inherited
 * from supertrait <code>FixtureSuite</code>).
 * This trait also inherits the abstract method <code>withFixture</code> from supertrait <code>FixtureSuite</code>. The <code>withFixture</code> method
 * takes a <code>OneArgTest</code>, which is a nested trait defined as a member of supertrait <code>FixtureSuite</code>.
 * <code>OneArgTest</code> has an <code>apply</code> method that takes a <code>Fixture</code>.
 * This <code>apply</code> method is responsible for running a test.
 * This trait's <code>runTest</code> method delegates the actual running of each test to <code>withFixture</code>, passing
 * in the test code to run via the <code>OneArgTest</code> argument. The <code>withFixture</code> method (abstract in this trait) is responsible
 * for creating the fixture and passing it to the test function.
 * </p>
 * 
 * <p>
 * Subclasses of this trait must, therefore, do three things differently from a plain old <code>org.scalatest.FunSuite</code>:
 * </p>
 * 
 * <ol>
 * <li>define the type of the fixture object by specifying type <code>Fixture</code></li>
 * <li>define the <code>withFixture</code> method</li>
 * <li>write tests that take a <code>Fixture</code> (You can also define tests that don't take a <code>Fixture</code>.)</li>
 * </ol>
 *
 * <p>
 * Here's an example:
 * </p>
 *
 * <pre>
 * import org.scalatest.fixture.FixtureFunSuite
 * import java.io.FileReader
 * import java.io.FileWriter
 * import java.io.File
 * 
 * class MyFunSuite extends FixtureFunSuite {
 *
 *   // 1. define type FixtureParam
 *   type FixtureParam = FileReader
 *
 *   // 2. define the withFixture method
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
 *   // 3. write tests that take a fixture parameter
 *   test("reading from the temp file") { reader =>
 *     var builder = new StringBuilder
 *     var c = reader.read()
 *     while (c != -1) {
 *       builder.append(c.toChar)
 *       c = reader.read()
 *     }
 *     assert(builder.toString === "Hello, test!")
 *   }
 * 
 *   test("first char of the temp file") { reader =>
 *     assert(reader.read() === 'H')
 *   }
 * 
 *   // (You can also write tests that don't take a fixture parameter.)
 *   test("without a fixture") { () =>
 *     assert(1 + 1 === 2)
 *   }
 * }
 * </pre>
 *
 * <p>
 * If the fixture you want to pass into your tests consists of multiple objects, you will need to combine
 * them into one object to use this trait. One good approach to passing multiple fixture objects is
 * to encapsulate them in a tuple. Here's an example that takes the tuple approach:
 * </p>
 *
 * <pre>
 * import org.scalatest.fixture.FixtureFunSuite
 * import scala.collection.mutable.ListBuffer
 *
 * class MyFunSuite extends FixtureFunSuite {
 *
 *   type FixtureParam = (StringBuilder, ListBuffer[String])
 *
 *   def withFixture(test: OneArgTest) {
 *
 *     // Create needed mutable objects
 *     val stringBuilder = new StringBuilder("ScalaTest is ")
 *     val listBuffer = new ListBuffer[String]
 *
 *     // Invoke the test function, passing in the mutable objects
 *     test(stringBuilder, listBuffer)
 *   }
 *
 *   test("easy") { fixture => 
 *     val (builder, buffer) = fixture
 *     builder.append("easy!")
 *     assert(builder.toString === "ScalaTest is easy!")
 *     assert(buffer.isEmpty)
 *     buffer += "sweet"
 *   }
 *
 *   test("fun") { fixture =>
 *     val (builder, buffer) = fixture
 *     builder.append("fun!")
 *     assert(builder.toString === "ScalaTest is fun!")
 *     assert(buffer.isEmpty)
 *   }
 * }
 * </pre>
 *
 * <p>
 * When using a tuple to pass multiple fixture objects, it is usually helpful to give names to each
 * individual object in the tuple with a pattern-match assignment, as is done at the beginning
 * of each test here with:
 * </p>
 *
 * <pre>
 * val (builder, buffer) = fixture
 * </pre>
 *
 * <p>
 * Another good approach to passing multiple fixture objects is
 * to encapsulate them in a case class. Here's an example that takes the case class approach:
 * </p>
 *
 * <pre>
 * import org.scalatest.fixture.FixtureFunSuite
 * import scala.collection.mutable.ListBuffer
 *
 * class MyFunSuite extends FixtureFunSuite {
 *
 *   case class FixtureHolder(builder: StringBuilder, buffer: ListBuffer[String])
 *
 *   type FixtureParam = FixtureHolder
 *
 *   def withFixture(test: OneArgTest) {
 *
 *     // Create needed mutable objects
 *     val stringBuilder = new StringBuilder("ScalaTest is ")
 *     val listBuffer = new ListBuffer[String]
 *
 *     // Invoke the test function, passing in the mutable objects
 *     test(FixtureHolder(stringBuilder, listBuffer))
 *   }
 *
 *   test("easy") { fixture =>
 *     import fixture._
 *     builder.append("easy!")
 *     assert(builder.toString === "ScalaTest is easy!")
 *     assert(buffer.isEmpty)
 *     buffer += "sweet"
 *   }
 *
 *   test("fun") { fixture =>
 *     fixture.builder.append("fun!")
 *     assert(fixture.builder.toString === "ScalaTest is fun!")
 *     assert(fixture.buffer.isEmpty)
 *   }
 * }
 * </pre>
 *
 * <p>
 * When using a case class to pass multiple fixture objects, it can be helpful to make the names of each
 * individual object available as a single identifier with an import statement. This is the approach
 * taken by the <code>testEasy</code> method in the previous example. Because it imports the members
 * of the fixture object, the test code can just use them as unqualified identifiers:
 * </p>
 *
 * <pre>
 * test("easy") { fixture =>
 *   import fixture._
 *   builder.append("easy!")
 *   assert(builder.toString === "ScalaTest is easy!")
 *   assert(buffer.isEmpty)
 *   buffer += "sweet"
 * }
 * </pre>
 *
 * <p>
 * Alternatively, you may sometimes prefer to qualify each use of a fixture object with the name
 * of the fixture parameter. This approach, taken by the <code>testFun</code> method in the previous
 * example, makes it more obvious which variables in your test 
 * are part of the passed-in fixture:
 * </p>
 *
 * <pre>
 * test("fun") { fixture =>
 *   fixture.builder.append("fun!")
 *   assert(fixture.builder.toString === "ScalaTest is fun!")
 *   assert(fixture.buffer.isEmpty)
 * }
 * </pre>
 *
 * <p>
 * <strong>Configuring fixtures and tests</strong>
 * </p>
 * 
 * <p>
 * Sometimes you may want to write tests that are configurable. For example, you may want to write
 * a suite of tests that each take an open temp file as a fixture, but whose file name is specified
 * externally so that the file name can be can be changed from run to run. To accomplish this
 * the <code>OneArgTest</code> trait has a <code>configMap</code>
 * method, which will return a <code>Map[String, Any]</code> from which configuration information may be obtained.
 * The <code>runTest</code> method of this trait will pass a <code>OneArgTest</code> to <code>withFixture</code>
 * whose <code>configMap</code> method returns the <code>configMap</code> passed to <code>runTest</code>.
 * Here's an example in which the name of a temp file is taken from the passed <code>configMap</code>:
 * </p>
 *
 * <pre>
 * import org.scalatest.fixture.FixtureFunSuite
 * import java.io.FileReader
 * import java.io.FileWriter
 * import java.io.File
 * 
 * class MyFunSuite extends FixtureFunSuite {
 *
 *   type FixtureParam = FileReader
 *
 *   def withFixture(test: OneArgTest) {
 *
 *     require(
 *       test.configMap.contains("TempFileName"),
 *       "This suite requires a TempFileName to be passed in the configMap"
 *     )
 *
 *     // Grab the file name from the configMap
 *     val FileName = test.configMap("TempFileName")
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
 *   test("reading from the temp file") { reader =>
 *     var builder = new StringBuilder
 *     var c = reader.read()
 *     while (c != -1) {
 *       builder.append(c.toChar)
 *       c = reader.read()
 *     }
 *     assert(builder.toString === "Hello, test!")
 *   }
 * 
 *   test("first char of the temp file") { reader =>
 *     assert(reader.read() === 'H')
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you want to pass into each test the entire <code>configMap</code> that was passed to <code>runTest</code>, you 
 * can mix in trait <code>ConfigMapFixture</code>. See the <a href="ConfigMapFixture.html">documentation
 * for <code>ConfigMapFixture</code></a> for the details, but here's a quick
 * example of how it looks:
 * </p>
 *
 * <pre>
 *  import org.scalatest.fixture.FixtureFunSuite
 *  import org.scalatest.fixture.ConfigMapFixture
 *
 *  class MyFunSuite extends FixtureFunSuite with ConfigMapFixture {
 *
 *    test("hello") { configMap =>
 *      // Use the configMap passed to runTest in the test
 *      assert(configMap.contains("hello")
 *    }
 *
 *    test("world") { configMap =>
 *      assert(configMap.contains("world")
 *    }
 *  }
 * </pre>
 *
 * <p>
 * <code>ConfigMapFixture</code> can also be used to facilitate writing <code>FixtureFunSuite</code>s that include tests
 * that take different fixture types. See the documentation for <a href="MultipleFixtureFunSuite.html"><code>MultipleFixtureFunSuite</code></a> for more information.
 * </p>
 *
 * @author Bill Venners
 */
trait FixtureFunSuite extends FixtureSuite { thisSuite =>

  private val IgnoreTagName = "org.scalatest.Ignore"

  private abstract class FunNode
  private case class TestNode(testName: String, fun: FixtureParam => Any) extends FunNode
  private case class InfoNode(message: String) extends FunNode

  // Access to the testNamesList, testsMap, and tagsMap must be synchronized, because the test methods are invoked by
  // the primary constructor, but testNames, tags, and runTest get invoked directly or indirectly
  // by run. When running tests concurrently with ScalaTest Runner, different threads can
  // instantiate and run the suite. Instead of synchronizing, I put them in an immutable Bundle object (and
  // all three collections--testNamesList, testsMap, and tagsMap--are immuable collections), then I put the Bundle
  // in an AtomicReference. Since the expected use case is the test method will be called
  // from the primary constructor, which will be all done by one thread, I just in effect use optimistic locking on the Bundle.
  // If two threads ever called test at the same time, they could get a ConcurrentModificationException.
  // Test names are in reverse order of test registration method invocations
  private class Bundle private(
    val testNamesList: List[String],
    val doList: List[FunNode],
    val testsMap: Map[String, TestNode],
    val tagsMap: Map[String, Set[String]],
    val registrationClosed: Boolean
  ) {
    def unpack = (testNamesList, doList, testsMap, tagsMap, registrationClosed)
  }

  private object Bundle {
    def apply(
      testNamesList: List[String],
      doList: List[FunNode],
      testsMap: Map[String, TestNode],
      tagsMap: Map[String, Set[String]],
      registrationClosed: Boolean
    ): Bundle =
      new Bundle(testNamesList, doList,testsMap, tagsMap, registrationClosed)
  }

  private val atomic = new AtomicReference[Bundle](Bundle(List(), List(), Map(), Map(), false))

  private def updateAtomic(oldBundle: Bundle, newBundle: Bundle) {
    val shouldBeOldBundle = atomic.getAndSet(newBundle)
    if (!(shouldBeOldBundle eq oldBundle))
      throw new ConcurrentModificationException(Resources("concurrentFixtureFunSuiteBundleMod"))
  }

  private class RegistrationInformer extends Informer {
    def apply(message: String) {
      if (message == null)
        throw new NullPointerException
      val oldBundle = atomic.get
      var (testNamesList, doList, testsMap, tagsMap, registrationClosed) = oldBundle.unpack
      doList ::= InfoNode(message)
      updateAtomic(oldBundle, Bundle(testNamesList, doList, testsMap, tagsMap, registrationClosed))
    }
  }

  // The informer will be a registration informer until run is called for the first time. (This
  // is the registration phase of a FixtureFunSuite's lifecycle.)
  private final val atomicInformer = new AtomicReference[Informer](new RegistrationInformer)

  /**
   * Returns an <code>Informer</code> that during test execution will forward strings (and other objects) passed to its
   * <code>apply</code> method to the current reporter. If invoked in a constructor, it
   * will register the passed string for forwarding later during test execution. If invoked while this
   * <code>FixtureFunSuite</code> is being executed, such as from inside a test function, it will forward the information to
   * the current reporter immediately. If invoked at any other time, it will
   * throw an exception. This method can be called safely by any thread.
   */
  implicit protected def info: Informer = atomicInformer.get

  private val zombieInformer =
    new Informer {
      private val complaint = Resources("cantCallInfoNow", "FixtureFunSuite")
      def apply(message: String) {
        if (message == null)
          throw new NullPointerException
        throw new IllegalStateException(complaint)
      }
    }

  /**
   * Register a test with the specified name, optional tags, and function value that takes no arguments.
   * This method will register the test for later execution via an invocation of one of the <code>run</code>
   * methods. The passed test name must not have been registered previously on
   * this <code>FunSuite</code> instance.
   *
   * @param testName the name of the test
   * @param testTags the optional list of tags for this test
   * @param testFun the test function
   * @throws TestRegistrationClosedException if invoked after <code>run</code> has been invoked on this suite
   * @throws DuplicateTestNameException if a test with the same name has been registered previously
   * @throws NotAllowedException if <code>testName</code> had been registered previously
   * @throws NullPointerException if <code>testName</code> or any passed test tag is <code>null</code>
   */
  protected def test(testName: String, testTags: Tag*)(f: FixtureParam => Any) {

    if (testName == null)
      throw new NullPointerException("testName was null")
    if (testTags.exists(_ == null))
      throw new NullPointerException("a test tag was null")

    if (atomic.get.registrationClosed)
      throw new TestRegistrationClosedException(Resources("testCannotAppearInsideAnotherTest"), getStackDepth("FunSuite.scala", "test"))

    if (atomic.get.testsMap.keySet.contains(testName))
      throw new DuplicateTestNameException(Resources("duplicateTestName", testName), getStackDepth("FunSuite.scala", "test"))

    val oldBundle = atomic.get
    var (testNamesList, doList, testsMap, tagsMap, registrationClosed) = oldBundle.unpack

    val testNode = TestNode(testName, f)
    testsMap += (testName -> testNode)
    testNamesList ::= testName
    doList ::= testNode
    val tagNames = Set[String]() ++ testTags.map(_.name)
    if (!tagNames.isEmpty)
      tagsMap += (testName -> tagNames)

    updateAtomic(oldBundle, Bundle(testNamesList, doList, testsMap, tagsMap, registrationClosed))
  }

  /**
   * Register a test to ignore, which has the specified name, optional tags, and function value that takes no arguments.
   * This method will register the test for later ignoring via an invocation of one of the <code>run</code>
   * methods. This method exists to make it easy to ignore an existing test by changing the call to <code>test</code>
   * to <code>ignore</code> without deleting or commenting out the actual test code. The test will not be run, but a
   * report will be sent that indicates the test was ignored. The passed test name must not have been registered previously on
   * this <code>FunSuite</code> instance.
   *
   * @param testName the name of the test
   * @param testTags the optional list of tags for this test
   * @param testFun the test function
   * @throws TestRegistrationClosedException if invoked after <code>run</code> has been invoked on this suite
   * @throws DuplicateTestNameException if a test with the same name has been registered previously
   * @throws NotAllowedException if <code>testName</code> had been registered previously
   */
  protected def ignore(testName: String, testTags: Tag*)(f: FixtureParam => Any) {

    if (testName == null)
      throw new NullPointerException("testName was null")
    if (testTags.exists(_ == null))
      throw new NullPointerException("a test tag was null")

    if (atomic.get.registrationClosed)
      throw new TestRegistrationClosedException(Resources("ignoreCannotAppearInsideATest"), getStackDepth("FunSuite.scala", "ignore"))

    test(testName)(f) // Call test without passing the tags

    val oldBundle = atomic.get
    var (testNamesList, doList, testsMap, tagsMap, registrationClosed) = oldBundle.unpack

    val tagNames = Set[String]() ++ testTags.map(_.name)
    tagsMap += (testName -> (tagNames + IgnoreTagName))

    updateAtomic(oldBundle, Bundle(testNamesList, doList, testsMap, tagsMap, registrationClosed))
  }

  /**
  * An immutable <code>Set</code> of test names. If this <code>FixtureFunSuite</code> contains no tests, this method returns an empty <code>Set</code>.
  *
  * <p>
  * This trait's implementation of this method will return a set that contains the names of all registered tests. The set's iterator will
  * return those names in the order in which the tests were registered.
  * </p>
  */
  override def testNames: Set[String] = {
    // I'm returning a ListSet here so that they tests will be run in registration order
    ListSet(atomic.get.testNamesList.toArray: _*)
  }

  // runTest should throw IAE if a test name is passed that doesn't exist. Looks like right now it just reports a test failure.
  /**
   * Run a test. This trait's implementation runs the test registered with the name specified by <code>testName</code>.
   *
   * @param testName the name of one test to run.
   * @param reporter the <code>Reporter</code> to which results will be reported
   * @param stopper the <code>Stopper</code> that will be consulted to determine whether to stop execution early.
   * @param configMap a <code>Map</code> of properties that can be used by the executing <code>Suite</code> of tests.
   * @throws NullPointerException if any of <code>testName</code>, <code>reporter</code>, <code>stopper</code>, or <code>configMap</code>
   *     is <code>null</code>.
   */
  protected override def runTest(testName: String, reporter: Reporter, stopper: Stopper, configMap: Map[String, Any], tracker: Tracker) {

    if (testName == null || reporter == null || stopper == null || configMap == null)
      throw new NullPointerException

    val stopRequested = stopper
    val report = wrapReporterIfNecessary(reporter)

    // Create a Rerunner if the FunSuite has a no-arg constructor
    val hasPublicNoArgConstructor = org.scalatest.Suite.checkForPublicNoArgConstructor(getClass)

    val rerunnable =
      if (hasPublicNoArgConstructor)
        Some(new TestRerunner(getClass.getName, testName))
      else
        None

    val testStartTime = System.currentTimeMillis
    report(TestStarting(tracker.nextOrdinal(), thisSuite.suiteName, Some(thisSuite.getClass.getName), testName, None, rerunnable))

    try {

      val theTest = atomic.get.testsMap(testName)

      val informerForThisTest =
        new ConcurrentInformer(NameInfo(thisSuite.suiteName, Some(thisSuite.getClass.getName), Some(testName))) {
          def apply(message: String) {
            if (message == null)
              throw new NullPointerException
            report(InfoProvided(tracker.nextOrdinal(), message, nameInfoForCurrentThread))
          }
        }

      val oldInformer = atomicInformer.getAndSet(informerForThisTest)
      var swapAndCompareSucceeded = false
      try {
        theTest.fun match {
          case wrapper: NoArgTestWrapper[_] =>
            withFixture(new FixturelessTestFunAndConfigMap(testName, wrapper.test, configMap))
          case fun => withFixture(new TestFunAndConfigMap(testName, fun, configMap))
        }
      }
      finally {
        val shouldBeInformerForThisTest = atomicInformer.getAndSet(oldInformer)
        swapAndCompareSucceeded = shouldBeInformerForThisTest eq informerForThisTest
      }
      if (!swapAndCompareSucceeded)  // Do outside finally to workaround Scala compiler bug
        throw new ConcurrentModificationException(Resources("concurrentInformerMod", thisSuite.getClass.getName))

      val duration = System.currentTimeMillis - testStartTime
      report(TestSucceeded(tracker.nextOrdinal(), thisSuite.suiteName, Some(thisSuite.getClass.getName), testName, Some(duration), None, rerunnable))
    }
    catch {
      case _: TestPendingException =>
        report(TestPending(tracker.nextOrdinal(), thisSuite.suiteName, Some(thisSuite.getClass.getName), testName))
      case e if !anErrorThatShouldCauseAnAbort(e) =>
        val duration = System.currentTimeMillis - testStartTime
        handleFailedTest(e, false, testName, rerunnable, report, tracker, duration)
      case e => throw e
    }
  }

  private def handleFailedTest(throwable: Throwable, hasPublicNoArgConstructor: Boolean, testName: String,
      rerunnable: Option[Rerunner], reporter: Reporter, tracker: Tracker, duration: Long) {

    val message =
      if (throwable.getMessage != null) // [bv: this could be factored out into a helper method]
        throwable.getMessage
      else
        throwable.toString

    reporter(TestFailed(tracker.nextOrdinal(), message, thisSuite.suiteName, Some(thisSuite.getClass.getName), testName, Some(throwable), Some(duration), None, rerunnable))
  }

  /**
   * A <code>Map</code> whose keys are <code>String</code> tag names to which tests in this <code>FunSuite</code> belong, and values
   * the <code>Set</code> of test names that belong to each tag. If this <code>FunSuite</code> contains no tags, this method returns an empty <code>Map</code>.
   *
   * <p>
   * This trait's implementation returns tags that were passed as strings contained in <code>Tag</code> objects passed to
   * methods <code>test</code> and <code>ignore</code>.
   * </p>
   */
  override def tags: Map[String, Set[String]] = atomic.get.tagsMap

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

    // Wrap any non-DispatchReporter, non-CatchReporter in a CatchReporter,
    // so that exceptions are caught and transformed
    // into error messages on the standard error stream.
    val report = wrapReporterIfNecessary(reporter)

    // If a testName is passed to run, just run that, else run the tests returned
    // by testNames.
    testName match {
      case Some(tn) => runTest(tn, report, stopRequested, configMap, tracker)
      case None =>

        val doList = atomic.get.doList.reverse
        for (node <- doList) {
          node match {
            case InfoNode(message) => info(message)
            case TestNode(tn, _) =>
              val (filterTest, ignoreTest) = filter(tn, tags)
              if (!filterTest)
                if (ignoreTest)
                  report(TestIgnored(tracker.nextOrdinal(), thisSuite.suiteName, Some(thisSuite.getClass.getName), tn))
                else
                  runTest(tn, report, stopRequested, configMap, tracker)
          }
        }
    }
  }

  override def run(testName: Option[String], reporter: Reporter, stopper: Stopper, filter: Filter,
      configMap: Map[String, Any], distributor: Option[Distributor], tracker: Tracker) {

    val stopRequested = stopper

    // Set the flag that indicates registration is closed (because run has now been invoked),
    // which will disallow any further invocations of "test" or "ignore" with
    // an RegistrationClosedException.
    val oldBundle = atomic.get
    val (testNamesList, doList, testsMap, tagsMap, registrationClosed) = oldBundle.unpack
    if (!registrationClosed)
      updateAtomic(oldBundle, Bundle(testNamesList, doList, testsMap, tagsMap, true))

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
   * Registers shared tests.
   *
   * <p>
   * This method enables the following syntax for shared tests in a <code>FixtureFunSuite</code>:
   * </p>
   *
   * <pre>
   * testsFor(nonEmptyStack(lastValuePushed))
   * </pre>
   *
   * <p>
   * This method just provides syntax sugar intended to make the intent of the code clearer.
   * Because the parameter passed to it is
   * type <code>Unit</code>, the expression will be evaluated before being passed, which
   * is sufficient to register the shared tests. For examples of shared tests, see the
   * <a href="../FunSuite.html#SharedTests">Shared tests section</a> in the main documentation for
   * trait <code>FunSuite</code>.
   * </p>
   */
  protected def testsFor(unit: Unit) {}

  /**
   * Implicitly converts a function that takes no parameters and results in <code>PendingNothing</code> to
   * a function from <code>Fixture</code> to <code>Any</code>, to enable pending tests to registered as by-name parameters
   * by methods that require a test function that takes a <code>Fixture</code>.
   *
   * <p>
   * This method makes it possible to write pending tests as simply <code>(pending)</code>, without needing
   * to write <code>(fixture => pending)</code>.
   * </p>
   */
  protected implicit def convertPendingToFixtureFunction(f: => PendingNothing): (FixtureParam => Any) = {
    fixture => f
  }

  /**
   * Implicitly converts a function that takes no parameters and results in <code>Any</code> to
   * a function from <code>Fixture</code> to <code>Any</code>, to enable no-arg tests to registered
   * by methods that require a test function that takes a <code>Fixture</code>.
   */
  protected implicit def convertNoArgToFixtureFunction(fun: () => Any): (FixtureParam => Any) =
    new NoArgTestWrapper(fun)
}
