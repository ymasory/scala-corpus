/**
 * Copyright (c) 2007-2010 Eric Torreborre <etorreborre@yahoo.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
 * documentation files (the "Software"), to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of
 * the Software. Neither the name of specs nor the names of its contributors may be used to endorse or promote
 * products derived from this software without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
package org.specs.runner
import org.scalatest.{ Suite, Stopper, Filter, Distributor, Tracker }
import org.scalatest.events._
import org.specs.specification._
import org.specs._
/**
 * Concrete class for the ScalaTest trait.
 * Usage:<code>
 * class mySpecRunner extends ScalaTestSuite(mySpec)
 * </code>
 * Then it can be run with the ScalaTest gui runner: <code>java -cp ... org.scalatest.Runner -g -s mySpecRunner</code>
 */
class ScalaTestSuite(specifications: Specification*) extends ScalaTest {
  val specs: Seq[Specification] = specifications
}

/**
 * This trait is a ScalaTest suite which is build from one or more specifications (provided by the inherited SpecsHolder)
 * The subspecifications, as well the system under test (sus) are provided as nested suites
 * Usage:<code>
 * class mySpecRunner extends Runner(mySpec) with ScalaTest
 * <code>
 */
trait ScalaTest extends SpecsFilter with FailOrSkip with org.scalatest.Suite {
  /**
   * @return the name of the suite which is either the specification name if there's only one or
   * a name build after <code>this</code> class
   */
  override def suiteName = {
    if (filteredSpecs.size > 1)
      this.getClass.getName.replaceAll("\\$", "")
    else
      filteredSpecs(0).description
  }

  /**
   * @return an empty map for now. The notion of group may be added later to specifications
   */
  override def tags: Map[String, Set[String]] = Map()

  /**
   * @return the subspecifications or the systems as ScalaTest suites
   */
  override def nestedSuites: List[org.scalatest.Suite] = {
    filteredSpecs flatMap { specification =>
      specification.subSpecifications.map(new ScalaTestSuite(_)) ++
      specification.systems.map(new SusSuite(_))
    }
  }

  /**
   * @return an empty set as a specification doesn't hold tests by itself
   */
  override def testNames = Set()

   /**
    * Convenience method: adds a new failure to the latest example<br>
    * Usage: <code>fail("this code should fail anyway")</code>
    */
   override def fail(m: String) = super[FailOrSkip].fail(m)

   /**
    * Convenience method: adds a new failure to the latest example. The failure message is "failure"<br>
    * Usage: <code>fail</code>
    */
   override def fail(): Nothing = super[FailOrSkip].fail()

   /**
    * Convenience method: adds a new skippedException to the latest example<br>
    * Usage: <code>skip("this example should be skipped")</code>
    */
   override def skip(m: String) = super[FailOrSkip].skip(m)

}

/**
 * This class is a ScalaTest suite which is build from a system under test
 * its subspecifications or its systems under test (sus)
 */
class SusSuite(sus: Sus) extends Suite {
  /**
   * @return the description of the sus with either "should" or "can"
   */
  override def suiteName = sus.description + " " + sus.verb

  /**
   * @return Nil. A system under test has no nested suites
   */
  override def nestedSuites: List[Suite] = Nil

  /**
   * @return the descriptions of the examples to report. Subexamples names are not returned and will be run with their parent example
   */
  override def testNames: Set[java.lang.String] = {
    sus.examples.map(_.description).toSet
  }
  private def current: Ordinal = new Ordinal(0)
  /**
   * Report the result of several examples, checking if they are included or not
   */
  override def runTests(testName: Option[java.lang.String],
                         reporter: org.scalatest.Reporter,
                         stopper: Stopper,
                         filter: Filter,
                         properties: Map[java.lang.String, Any],
                         distributor: Option[Distributor],
                         tracker: Tracker): Unit = {
      val testTags = tags
      def isIncluded(name: String): Boolean = {
        val tagsForName = testTags.get(name).getOrElse(Set())
        val r = filter.tagsToInclude.isEmpty && 
                !filter.tagsToExclude.exists(tagsForName.contains(_)) ||
                !filter.tagsToInclude.isEmpty && 
                 filter.tagsToInclude.get.exists(tagsForName.contains(_)) &&
                !filter.tagsToExclude.exists(tagsForName.contains(_))
        r
      }
      testName filter(isIncluded(_)) map { name => 
        runTest(name, reporter, stopper, properties, tracker) 
      } getOrElse {
        testNames filter(isIncluded(_)) map { name => runTest(name, reporter, stopper, properties, tracker) }
      }
    }
    /**
     * Report the result of an example given its description to the reporter.
     */
    override def runTest(testName: java.lang.String,
                         reporter: org.scalatest.Reporter,
                         stopper: Stopper,
                         properties: Map[java.lang.String, Any],
                         tracker: Tracker): Unit = {
      sus.examples.find(_.description == testName).map(e => runExample(e, reporter, sus.description, properties))
    }

  /**
   * Report the result of an example: ignored if it is skipped, failed if it has failures or errors, succeeded otherwise
   * call this method recursively if the example has subexamples
   */
  private[this] def runExample(e: Example, reporter: org.scalatest.Reporter, suiteName: String, properties: Map[java.lang.String, Any]): Unit = {
    def planOnly = properties.keySet.contains("plan")
    if (planOnly)
      reporter(TestStarting(current.next, suiteName, None, "- " + e.description))
    else
      reporter(TestStarting(current.next, suiteName, None, e.statusAsText + " " + e.description))
    if (!planOnly) {
      e.skipped foreach { skipped =>
        reporter(TestIgnored(current.next, suiteName, None, e.description + ": " + skipped.message))
      }
      e.failures foreach { f =>
        reporter(TestFailed(current.next, f.getMessage, suiteName, None, e.description, Some(f)))
      }
      e.errors foreach { error =>
        reporter(TestFailed(current.next, error.getMessage, suiteName, None, e.description, Some(error)))
      }
      if (e.failures.isEmpty && e.errors.isEmpty && e.skipped.isEmpty)
        reporter(TestSucceeded(current.next, suiteName, None, e.description))
      e.examples foreach { sub => runExample(sub, reporter, suiteName, properties) }
    }
    else
      reporter(TestSucceeded(current.next, suiteName, None, e.description))
  }
  import scala.collection.immutable._
  /**
   * @return a map with the keys being the examples names and the values the set of tags for that name
   */
  override def tags: Map[String, Set[String]] = {
    var exampleNames: Map[String, Set[String]] = new HashMap[String, Set[String]]()
    for {e <- sus.examples
         tag <- e.tagNames } {
        val exampleTags: Set[String] = exampleNames.get(e.description) match {
          case None => new HashSet[String]
          case Some(set) => set
        }
        exampleNames  = exampleNames  + (e.description -> (exampleTags + tag))
    }
    exampleNames
  }
}