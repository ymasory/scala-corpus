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
package org.scalatest.fixture

import org.scalatest._
import collection.immutable.TreeSet
import java.lang.reflect.{InvocationTargetException, Method, Modifier}
import org.scalatest.Suite.checkForPublicNoArgConstructor
import org.scalatest.Suite.TestMethodPrefix
import org.scalatest.Suite.IgnoreAnnotation
import org.scalatest.Suite.InformerInParens
import FixtureSuite.FixtureAndInformerInParens
import FixtureSuite.FixtureInParens
import FixtureSuite.testMethodTakesAFixtureAndInformer
import FixtureSuite.testMethodTakesAnInformer
import FixtureSuite.testMethodTakesAFixture
import FixtureSuite.simpleNameForTest
import FixtureSuite.argsArrayForTestName
import org.scalatest.events._
import Suite.anErrorThatShouldCauseAnAbort

/**
 * <code>Suite</code> that can pass a fixture object into its tests.
 *
 * <p>
 * This trait behaves similarly to trait <code>org.scalatest.Suite</code>, except that tests may have a fixture parameter. The type of the
 * fixture parameter is defined by the abstract <code>FixtureParam</code> type, which is declared as a member of this trait.
 * This trait also declares an abstract <code>withFixture</code> method. This <code>withFixture</code> method
 * takes a <code>OneArgTest</code>, which is a nested trait defined as a member of this trait.
 * <code>OneArgTest</code> has an <code>apply</code> method that takes a <code>FixtureParam</code>.
 * This <code>apply</code> method is responsible for running a test.
 * This trait's <code>runTest</code> method delegates the actual running of each test to <code>withFixture</code>, passing
 * in the test code to run via the <code>OneArgTest</code> argument. The <code>withFixture</code> method (abstract in this trait) is responsible
 * for creating the fixture argument and passing it to the test function.
 * </p>
 * 
 * <p>
 * Subclasses of this trait must, therefore, do three things differently from a plain old <code>org.scalatest.Suite</code>:
 * </p>
 * 
 * <ol>
 * <li>define the type of the fixture parameter by specifying type <code>FixtureParam</code></li>
 * <li>define the <code>withFixture(OneArgTest)</code> method</li>
 * <li>write test methods that take a fixture parameter (You can also define test methods that don't take a fixture parameter.)</li>
 * </ol>
 *
 * <p>
 * Here's an example:
 * </p>
 *
 * <pre>
 * import org.scalatest.fixture.FixtureSuite
 * import java.io.FileReader
 * import java.io.FileWriter
 * import java.io.File
 * 
 * class MySuite extends FixtureSuite {
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
 *   // 3. write test methods that take a fixture parameter
 *   def testReadingFromTheTempFile(reader: FileReader) {
 *     var builder = new StringBuilder
 *     var c = reader.read()
 *     while (c != -1) {
 *       builder.append(c.toChar)
 *       c = reader.read()
 *     }
 *     assert(builder.toString === "Hello, test!")
 *   }
 * 
 *   def testFirstCharOfTheTempFile(reader: FileReader) {
 *     assert(reader.read() === 'H')
 *   }
 * 
 *   // (You can also write tests methods that don't take a fixture parameter.)
 *   def testWithoutAFixture() { 
 *     without fixture {
 *       assert(1 + 1 === 2)
 *     }
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
 * import org.scalatest.fixture.FixtureSuite
 * import scala.collection.mutable.ListBuffer
 *
 * class MySuite extends FixtureSuite {
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
 *   def testEasy(fixture: Fixture) {
 *     val (builder, buffer) = fixture
 *     builder.append("easy!")
 *     assert(builder.toString === "ScalaTest is easy!")
 *     assert(buffer.isEmpty)
 *     buffer += "sweet"
 *   }
 *
 *   def testFun(fixture: Fixture) {
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
 * of each test method here with:
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
 * import org.scalatest.fixture.FixtureSuite
 * import scala.collection.mutable.ListBuffer
 *
 * class MySuite extends FixtureSuite {
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
 *   def testEasy(fixture: Fixture) {
 *     import fixture._
 *     builder.append("easy!")
 *     assert(builder.toString === "ScalaTest is easy!")
 *     assert(buffer.isEmpty)
 *     buffer += "sweet"
 *   }
 *
 *   def testFun(fixture: Fixture) {
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
 * of the fixture object, the test method code can just use them as unqualified identifiers:
 * </p>
 *
 * <pre>
 * def testEasy(fixture: Fixture) {
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
 * example, makes it more obvious which variables in your test method
 * are part of the passed-in fixture:
 * </p>
 *
 * <pre>
 * def testFun(fixture: Fixture) {
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
 * import org.scalatest.fixture.FixtureSuite
 * import java.io.FileReader
 * import java.io.FileWriter
 * import java.io.File
 * 
 * class MySuite extends FixtureSuite {
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
 *   def testReadingFromTheTempFile(reader: FileReader) {
 *     var builder = new StringBuilder
 *     var c = reader.read()
 *     while (c != -1) {
 *       builder.append(c.toChar)
 *       c = reader.read()
 *     }
 *     assert(builder.toString === "Hello, test!")
 *   }
 * 
 *   def testFirstCharOfTheTempFile(reader: FileReader) {
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
 *  import org.scalatest.fixture.FixtureSuite
 *  import org.scalatest.fixture.ConfigMapFixture
 *
 *  class MySuite extends FixtureSuite with ConfigMapFixture {
 *
 *    def testHello(configMap: Map[String, Any]) {
 *      // Use the configMap passed to runTest in the test
 *      assert(configMap.contains("hello")
 *    }
 *
 *    def testWorld(configMap: Map[String, Any]) {
 *      assert(configMap.contains("world")
 *    }
 *  }
 * </pre>
 *
 * <p>
 * Note: because a <code>FixtureSuite</code>'s test methods are invoked with reflection at runtime, there is no good way to
 * create a <code>FixtureSuite</code> containing test methods that take different fixtures. If you find you need to do this,
 * you may want to split your class into multiple <code>FixtureSuite</code>s, each of which contains test methods that take the
 * common <code>Fixture</code> type defined in that class, or use a <a href="MultipleFixtureFunSuite.html"><code>MultipleFixtureFunSuite</code></a>. 
 * </p>
 *
 * @author Bill Venners
 */
trait FixtureSuite extends org.scalatest.Suite { thisSuite =>

  /**
   * The type of the fixture parameter that can be passed into tests in this suite.
   */
  protected type FixtureParam

  /**
   * Trait whose instances encapsulate a test function that takes a fixture and config map.
   *
   * <p>
   * The <code>FixtureSuite</code> trait's implementation of <code>runTest</code> passes instances of this trait
   * to <code>FixtureSuite</code>'s <code>withFixture</code> method, such as:
   * </p>
   *
   * <pre>
   * def testSomething(fixture: Fixture) {
   *   // ...
   * }
   * def testSomethingElse(fixture: Fixture, info: Informer) {
   *   // ...
   * }
   * </pre>
   *
   * <p>
   * For more detail and examples, see the
   * <a href="FixtureSuite.html">documentation for trait <code>FixtureSuite</code></a>.
   * </p>
   */
  protected trait OneArgTest extends (FixtureParam => Unit) {

    /**
     * The name of this test.
     */
    def name: String

    /**
     * Run the test, using the passed <code>FixtureParam</code>.
     */
    def apply(fixture: FixtureParam)

    /**
     * Return a <code>Map[String, Any]</code> containing objects that can be used
     * to configure the fixture and test.
     */
    def configMap: Map[String, Any]
  }

  /*
   * Trait whose instances encapsulate a test function that takes no fixture and config map.
   *
   * <p>
   * The <code>FixtureSuite</code> trait's implementation of <code>runTest</code> passes instances of this trait
   * to <code>FixtureSuite</code>'s <code>withFixture</code> method for test methods that take no
   * fixture, such as:
   * </p>
   *
   * <pre>
   * def testSomething() {
   *   // ...
   * }
   * def testSomethingElse(info: Informer) {
   *   // ...
   * }
   * </pre>
   *
   * <p>
   * This trait enables <code>withFixture</code> method implementatinos to detect test that
   * don't require a fixture. If a fixture is expensive to create and cleanup, <code>withFixture</code>
   * method implementations can opt to not create fixtures for tests that don't need them.
   * For more detail and examples, see the
   * <a href="FixtureSuite.html">documentation for trait <code>FixtureSuite</code></a>.
   * </p>
   */
  /* protected trait FixturelessTest extends OneArgTest with (() => Unit) {

    /**
     * Run the test that takes no <code>Fixture</code>.
     */
    def apply()
  } */

  /**
   *  Run the passed test function with a fixture created by this method.
   *
   * <p>
   * This method should create the fixture object needed by the tests of the
   * current suite, invoke the test function (passing in the fixture object),
   * and if needed, perform any clean up needed after the test completes.
   * For more detail and examples, see the <a href="FixtureSuite.html">main documentation for this trait</a>.
   * </p>
   *
   * @param fun the <code>OneArgTest</code> to invoke, passing in a fixture
   */
  protected def withFixture(test: OneArgTest)

  private[fixture] class TestFunAndConfigMap(val name: String, test: FixtureParam => Any, val configMap: Map[String, Any])
    extends OneArgTest {
    
    def apply(fixture: FixtureParam) {
      test(fixture)
    }
  }

  private[fixture] class FixturelessTestFunAndConfigMap(override val name: String, test: () => Any, override val configMap: Map[String, Any])
    extends NoArgTest {

    def apply() { test() }
  }

  // Need to override this one becaue it call getMethodForTestName
  override def tags: Map[String, Set[String]] = {

    def getTags(testName: String) =
/* AFTER THE DEPRECATION CYCLE FOR GROUPS TO TAGS (0.9.8), REPLACE THE FOLLOWING FOR LOOP WITH THIS COMMENTED OUT ONE
   THAT MAKES SURE ANNOTATIONS ARE TAGGED WITH TagAnnotation.
      for {
        a <- getMethodForTestName(testName).getDeclaredAnnotations
        annotationClass = a.annotationType
        if annotationClass.isAnnotationPresent(classOf[TagAnnotation])
      } yield annotationClass.getName
*/
      for (a <- getMethodForTestName(testName).getDeclaredAnnotations)
        yield a.annotationType.getName

    val elements =
      for (testName <- testNames; if !getTags(testName).isEmpty)
        yield testName -> (Set() ++ getTags(testName))

    Map() ++ elements
  }

  override def testNames: Set[String] = {

    def takesInformer(m: Method) = {
      val paramTypes = m.getParameterTypes
      paramTypes.length == 1 && classOf[Informer].isAssignableFrom(paramTypes(0))
    }

    def takesTwoParamsOfTypesAnyAndInformer(m: Method) = {
      val paramTypes = m.getParameterTypes
      val hasTwoParams = paramTypes.length == 2
      hasTwoParams && classOf[Informer].isAssignableFrom(paramTypes(1))
    }

    def takesOneParamOfAnyType(m: Method) = m.getParameterTypes.length == 1

    def isTestMethod(m: Method) = {

      val isInstanceMethod = !Modifier.isStatic(m.getModifiers())

      // name must have at least 4 chars (minimum is "test")
      val simpleName = m.getName
      val firstFour = if (simpleName.length >= 4) simpleName.substring(0, 4) else "" 

      val paramTypes = m.getParameterTypes
      val hasNoParams = paramTypes.length == 0

      // Discover testNames(Informer) because if we didn't it might be confusing when someone
      // actually wrote a testNames(Informer) method and it was silently ignored.
      val isTestNames = simpleName == "testNames"

      // Also, will discover both
      // testNames(Object) and testNames(Object, Informer). Reason is if I didn't discover these
      // it would likely just be silently ignored, and that might waste users' time
      isInstanceMethod && (firstFour == "test") && ((hasNoParams && !isTestNames) ||
          takesInformer(m) || takesOneParamOfAnyType(m) || takesTwoParamsOfTypesAnyAndInformer(m))
    }

    val testNameArray =
      for (m <- getClass.getMethods; if isTestMethod(m)) yield
        if (takesInformer(m))
          m.getName + InformerInParens
        else if (takesOneParamOfAnyType(m))
          m.getName + FixtureInParens
        else if (takesTwoParamsOfTypesAnyAndInformer(m))
          m.getName + FixtureAndInformerInParens
        else m.getName

    TreeSet[String]() ++ testNameArray
  }

  protected override def runTest(testName: String, reporter: Reporter, stopper: Stopper, configMap: Map[String, Any], tracker: Tracker) {

    if (testName == null || reporter == null || stopper == null || configMap == null || tracker == null)
      throw new NullPointerException

    val stopRequested = stopper
    val report = wrapReporterIfNecessary(reporter)
    val method = getMethodForTestName(testName)

    // Create a Rerunner if the Suite has a no-arg constructor
    val hasPublicNoArgConstructor = checkForPublicNoArgConstructor(getClass)

    val rerunnable =
      if (hasPublicNoArgConstructor)
        Some(new TestRerunner(getClass.getName, testName))
      else
        None

    val testStartTime = System.currentTimeMillis

    report(TestStarting(tracker.nextOrdinal(), thisSuite.suiteName, Some(thisSuite.getClass.getName), testName, None, rerunnable))


    try {
      if (testMethodTakesAFixtureAndInformer(testName) || testMethodTakesAFixture(testName)) {
        val testFun: FixtureParam => Unit = {
          (fixture: FixtureParam) => {
            val anyRefFixture: AnyRef = fixture.asInstanceOf[AnyRef] // TODO zap this cast
            val args: Array[Object] =
              if (testMethodTakesAFixtureAndInformer(testName)) {
                val informer =
                  new Informer {
                    def apply(message: String) {
                      if (message == null)
                        throw new NullPointerException
                      report(InfoProvided(tracker.nextOrdinal(), message, Some(NameInfo(thisSuite.suiteName, Some(thisSuite.getClass.getName), Some(testName)))))
                    }
                  }
                Array(anyRefFixture, informer)
              }
              else
                Array(anyRefFixture)

            method.invoke(thisSuite, args: _*)
          }
        }
        withFixture(new TestFunAndConfigMap(testName, testFun, configMap))
      }
      else { // Test method does not take a fixture
        val testFun: () => Unit = {
          () => {
            val args: Array[Object] =
              if (testMethodTakesAnInformer(testName)) {
                val informer =
                  new Informer {
                    def apply(message: String) {
                      if (message == null)
                        throw new NullPointerException
                      report(InfoProvided(tracker.nextOrdinal(), message, Some(NameInfo(thisSuite.suiteName, Some(thisSuite.getClass.getName), Some(testName)))))
                    }
                  }
                Array(informer)
              }
              else
                Array()

            method.invoke(this, args: _*)
          }
        }
        withFixture(new FixturelessTestFunAndConfigMap(testName, testFun, configMap))
      }

      val duration = System.currentTimeMillis - testStartTime
      report(TestSucceeded(tracker.nextOrdinal(), thisSuite.suiteName, Some(thisSuite.getClass.getName), testName, Some(duration), None, rerunnable))
    }
    catch { 
      case ite: InvocationTargetException =>
        val t = ite.getTargetException
        t match {
          case _: TestPendingException =>
            report(TestPending(tracker.nextOrdinal(), thisSuite.suiteName, Some(thisSuite.getClass.getName), testName))
          case e if !anErrorThatShouldCauseAnAbort(e) =>
            val duration = System.currentTimeMillis - testStartTime
            handleFailedTest(t, hasPublicNoArgConstructor, testName, rerunnable, report, tracker, duration)
          case e => throw e
        }
      case e if !anErrorThatShouldCauseAnAbort(e) =>
        val duration = System.currentTimeMillis - testStartTime
        handleFailedTest(e, hasPublicNoArgConstructor, testName, rerunnable, report, tracker, duration)
      case e => throw e
    }
  }

  // TODO: This is identical with the one in Suite. Factor it out to an object somewhere.
  private def handleFailedTest(throwable: Throwable, hasPublicNoArgConstructor: Boolean, testName: String,
      rerunnable: Option[Rerunner], report: Reporter, tracker: Tracker, duration: Long) {

    val message =
      if (throwable.getMessage != null) // [bv: this could be factored out into a helper method]
        throwable.getMessage
      else
        throwable.toString

    report(TestFailed(tracker.nextOrdinal(), message, thisSuite.suiteName, Some(thisSuite.getClass.getName), testName, Some(throwable), Some(duration), None, rerunnable))
  }

  private def getMethodForTestName(testName: String) = {
    val candidateMethods = getClass.getMethods.filter(_.getName == simpleNameForTest(testName))
    val found =
      if (testMethodTakesAFixtureAndInformer(testName))
        candidateMethods.find(
          candidateMethod => {
            val paramTypes = candidateMethod.getParameterTypes
            paramTypes.length == 2 && paramTypes(1) == classOf[Informer]
          }
        )
      else if (testMethodTakesAnInformer(testName))
        candidateMethods.find(
          candidateMethod => {
            val paramTypes = candidateMethod.getParameterTypes
            paramTypes.length == 1 && paramTypes(0) == classOf[Informer]
          }
        )
      else if (testMethodTakesAFixture(testName))
        candidateMethods.find(
          candidateMethod => {
            val paramTypes = candidateMethod.getParameterTypes
            paramTypes.length == 1
          }
        )
      else
        candidateMethods.find(_.getParameterTypes.length == 0)

     found match {
       case Some(method) => method
       case None =>
         throw new IllegalArgumentException(Resources("testNotFound", testName))
     }
  }

   /*
  /*
   * Object that encapsulates a test function, which does not take a fixture,
   * and a config map.
   *
   * <p>
   * The <code>FixtureSuite</code> trait's implementation of <code>runTest</code> passes instances of this trait
   * to <code>FixtureSuite</code>'s <code>withFixture</code> method for tests that do not require a fixture to
   * be passed.  For more detail and examples, see the
   * <a href="FixtureSuite.html">documentation for trait <code>FixtureSuite</code></a>.
   * </p>
   */
  protected trait NoArgTestFunction extends (FixtureParam => Any) {

    /**
     * Run the test, ignoring the passed <code>Fixture</code>.
     *
     * <p>
     * This traits implementation of this method invokes the overloaded form
     * of <code>apply</code> that takes no parameters.
     * </p>
     */
    final def apply(fixture: Fixture): Any = {
      apply()
    }

    /**
     * Run the test without a <code>Fixture</code>.
     */
    def apply()
  }

  protected class WithoutWord {
    def fixture(fun: => Any): NoArgTestFunction = {
      new NoArgTestFunction {
        def apply() { fun }
      }
    }
  }

  protected def without = new WithoutWord  */
}

private object FixtureSuite {

  val FixtureAndInformerInParens = "(FixtureParam, Informer)"
  val FixtureInParens = "(FixtureParam)"

  private def testMethodTakesAFixtureAndInformer(testName: String) = testName.endsWith(FixtureAndInformerInParens)
  private def testMethodTakesAnInformer(testName: String) = testName.endsWith(InformerInParens)
  private def testMethodTakesAFixture(testName: String) = testName.endsWith(FixtureInParens)

  private def simpleNameForTest(testName: String) =
    if (testName.endsWith(FixtureAndInformerInParens))
      testName.substring(0, testName.length - FixtureAndInformerInParens.length)
    else if (testName.endsWith(FixtureInParens))
      testName.substring(0, testName.length - FixtureInParens.length)
    else if (testName.endsWith(InformerInParens))
      testName.substring(0, testName.length - InformerInParens.length)
    else
      testName

  private def argsArrayForTestName(testName: String): Array[Class[_]] =
    if (testMethodTakesAFixtureAndInformer(testName))
      Array(classOf[Object], classOf[Informer])
    else
      Array(classOf[Informer])
}
