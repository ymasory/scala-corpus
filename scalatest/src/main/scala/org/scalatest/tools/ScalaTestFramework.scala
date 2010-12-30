package org.scalatest.tools

import org.scalatools.testing._
import org.scalatest.tools.Runner.parsePropertiesArgsIntoMap
import org.scalatest.tools.Runner.parseCompoundArgIntoSet

/**
 * Class that makes ScalaTest tests visible to sbt.
 *
 * @author Josh Cough
 * @author Bill Venners
 */
class ScalaTestFramework extends Framework {

  /**
   * Returns <code>"ScalaTest"</code>, the human readable name for this test framework.
   */
  def name = "ScalaTest"

  /**
   * Returns an array containing one <code>org.scalatools.testing.TestFingerprint</code> object, whose superclass name is <code>org.scalatest.Suite</code>
   * and <code>isModule</code> value is false.
   */
  def tests =
    Array(
      new org.scalatools.testing.TestFingerprint {
        def superClassName = "org.scalatest.Suite"
        def isModule = false
      }
    )

  /**
   * Returns an <code>org.scalatools.testing.Runner</code> that will load test classes via the passed <code>testLoader</code>
   * and direct output from running the tests to the passed array of <code>Logger</code>s.
   */
  def testRunner(testLoader: ClassLoader, loggers: Array[Logger]) = {
    new ScalaTestRunner(testLoader, loggers)
  }

  /**The test runner for ScalaTest suites. It is compiled in a second step after the rest of sbt.*/
  private[tools] class ScalaTestRunner(val testLoader: ClassLoader, val loggers: Array[Logger]) extends org.scalatools.testing.Runner {

    import org.scalatest._

    def run(testClassName: String, fingerprint: TestFingerprint, eventHandler: EventHandler, args: Array[String]) {
      val testClass = Class.forName(testClassName, true, testLoader).asSubclass(classOf[Suite])

      if (isAccessibleSuite(testClass)) {

        val (propertiesArgsList, includesArgsList,
        excludesArgsList, repoArg) = parsePropsAndTags(args.filter(!_.equals("")))
        val propertiesMap: Map[String, String] = parsePropertiesArgsIntoMap(propertiesArgsList)
        val tagsToInclude: Set[String] = parseCompoundArgIntoSet(includesArgsList, "-n")
        val tagsToExclude: Set[String] = parseCompoundArgIntoSet(excludesArgsList, "-l")
        val filter = org.scalatest.Filter(if (tagsToInclude.isEmpty) None else Some(tagsToInclude), tagsToExclude)

        val (presentAllDurations, presentInColor, presentTestFailedExceptionStackTraces) =
          repoArg match {
            case Some(arg) => (
              arg contains 'D',
              !(arg contains 'W'),
              arg contains 'F'
             )
             case None => (false, true, false)
          }

        //  def run(testName: Option[String], reporter: Reporter, stopper: Stopper, filter: Filter,
        //              configMap: Map[String, Any], distributor: Option[Distributor], tracker: Tracker) {
        val repo = new ScalaTestReporter(eventHandler, presentAllDurations, presentInColor, presentTestFailedExceptionStackTraces)
        testClass.newInstance.run(None, repo, new Stopper {},
          filter, propertiesMap, None, new Tracker)
      }
      else throw new IllegalArgumentException("Class is not an accessible org.scalatest.Suite: " + testClassName)
    }

    private val emptyClassArray = new Array[java.lang.Class[T] forSome {type T}](0)

    private def isAccessibleSuite(clazz: java.lang.Class[_]): Boolean = {
      import java.lang.reflect.Modifier

      try {
        classOf[Suite].isAssignableFrom(clazz) &&
                Modifier.isPublic(clazz.getModifiers) &&
                !Modifier.isAbstract(clazz.getModifiers) &&
                Modifier.isPublic(clazz.getConstructor(emptyClassArray: _*).getModifiers)
      } catch {
        case nsme: NoSuchMethodException => false
        case se: SecurityException => false
      }
    }

/*
    private def logTrace(t: Throwable) = loggers.foreach(_ trace t)

    private def logError(msg: String) = loggers.foreach(_ error msg)

    private def logWarn(msg: String) = loggers.foreach(_ warn msg)

    private def logInfo(msg: String) = loggers.foreach(_ info msg)
*/

    private class ScalaTestReporter(eventHandler: EventHandler, presentAllDurations: Boolean,
        presentInColor: Boolean, presentTestFailedExceptionStackTraces: Boolean) extends StringReporter(
        presentAllDurations, presentInColor, presentTestFailedExceptionStackTraces) {

      import org.scalatest.events._

      protected def printPossiblyInColor(text: String, ansiColor: String) {
        import PrintReporter.ansiReset
        loggers.foreach { logger =>
          logger.info(if (logger.ansiCodesSupported && presentInColor) ansiColor + text + ansiReset else text)
        }
      }

      def dispose() = ()

      def fireEvent(tn: String, r: Result, e: Option[Throwable]) = {
/*
        r match {
          case Result.Skipped => logInfo("Test Skipped: " + tn)
          case Result.Failure =>
            logError("Test Failed: " + tn)
            e.foreach {logTrace(_)}
          case Result.Success => logInfo("Test Passed: " + tn)
        }
*/
        eventHandler.handle(
          new org.scalatools.testing.Event {
            def testName = tn
            def description = tn
            def result = r
            def error = e getOrElse null
          }
        )
      }

      override def apply(event: Event) {

        // Superclass will call printPossiblyInColor
        super.apply(event)

        // Logging done, all I need to do now is fire events
        event match {
          // the results of running an actual test
          case t: TestPending => fireEvent(t.testName, Result.Skipped, None)
          case t: TestFailed => fireEvent(t.testName, Result.Failure, t.throwable)
          case t: TestSucceeded => fireEvent(t.testName, Result.Success, None)
          case t: TestIgnored => fireEvent(t.testName, Result.Skipped, None)
          case _ => 
        }
      }
    }

    private[scalatest] def parsePropsAndTags(args: Array[String]) = {

      import collection.mutable.ListBuffer

      val props = new ListBuffer[String]()
      val includes = new ListBuffer[String]()
      val excludes = new ListBuffer[String]()
      var repoArg: Option[String] = None

      val it = args.elements
      while (it.hasNext) {

        val s = it.next

        if (s.startsWith("-D")) {
          props += s
        }
        else if (s.startsWith("-n")) {
          includes += s
          if (it.hasNext)
            includes += it.next
        }
        else if (s.startsWith("-l")) {
          excludes += s
          if (it.hasNext)
            excludes += it.next
        }
        else if (s.startsWith("-o")) {
          if (repoArg.isEmpty) // Just use first one. Ignore any others.
            repoArg = Some(s)
        }
        //      else if (s.startsWith("-t")) {
        //
        //        testNGXMLFiles += s
        //        if (it.hasNext)
        //          testNGXMLFiles += it.next
        //      }
        else {
          throw new IllegalArgumentException("Unrecognized argument: " + s)
        }
      }
      (props.toList, includes.toList, excludes.toList, repoArg)
    }
  }
}
