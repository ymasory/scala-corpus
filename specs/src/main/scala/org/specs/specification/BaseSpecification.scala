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
package org.specs.specification
import org.specs.matcher.MatcherUtils._
import org.specs.SpecUtils._
import scala.reflect.ClassManifest
import org.specs.execute._
import org.specs.util._
import org.specs.util.Control._
import org.specs.util.ExtendedString._
import org.specs.Specification
import org.specs.util.ExtendedThrowable._
/**
 * This class provides the base structure of a specification.<br>
 * A specification has a name, a description and is composed of:<ul>
 * <li>sub specifications and/or</li>
 * <li>systems under specification (systems)</li>
 * </ul>
 * <p/>
 * In turn the systems can contain recursively contain examples.
 * <p/>
 * The description of a specification is its class name by default but which can be also overriden for better readibility.
 * For example, it is possible to declare a Specification using a constructor taking the full name of the specification:
 * <code>
 * class messagingSpec extends Specification("Specification for the messaging system")
 * </code>
 * <p/>
 *
 * A <code>BaseSpecification</code> implements several traits:<ul>
 * <li>TreeNode: allowing the specification, its systems and examples to be considered as a generic tree of nodes</li>
 * <li>SpecificationSystems: trait holding the systems declaration and navigation functions</li>
 * <li>SpecificationExecutor: this enables the execution of examples in isolation, by executing them in a clone of the specification, so that any local variable used
 * by the example is set to its initial value as if other examples never had modified it. This trait specialize the ExampleLifeCycle trait defining all steps of an example execution</li>
 * <li>ExampleExpectationsListener: this trait allows an expectation to be added to the current example being executed</li>
 * <li>Tagged: allow to tag the specification with some name so that accepted and rejected tags define what should be executed or not</li>
 * <li>HasResults: generic trait for anything declaring failures, successes, errors and skipped</li>
 * <li>LinkedSpecification: this allow the declaration of links between literal specifications</li>
 * <li>SpecificationConfiguration: this defines variables which affect the behaviour of the specification. For example if examples without expectations 
 * should be marked as pending</li>
 * </ul>
 */
class BaseSpecification extends TreeNode with SpecificationSystems with SpecificationExecutor with ExampleExpectationsListener with Tagged 
  with HasResults with LinkedSpecification with SpecificationConfiguration 
  with ComposedSpecifications with LazyParameters { outer =>
    
  /** name of the specification */
  var name = createDescription(getClass.getName)
  /** description of the specification */
  var description = createDescription(getClass.getName)

  /**
   * @return a description from the class name, taking the last name which doesn't contain a $ or a number.
   * For example: com.pack1.MyClass$1$ will:<ul>
   * <li>split on $ and reverse: [1, com.pack1.MyClass]
   * <li>drop the every element which is an integer -> [com.pack1.MyClass]
   * <li>take the first element: com.pack1.MyClass
   * <li>split on . and reverse: [MyClass, pack1, com]
   * <li>take the last element: MyClass</ul>
   */
  private[specs] def createDescription(s: String) = s.
    split("\\$").reverse.
    dropWhile(isInteger(_))(0).
    split("\\.").
    reverse.toList(0)

  /** specifications contained by the current specification. An empty list by default */
  var subSpecifications: List[Specification] = List()
  /** specification which includes this one */
  var parentSpecification: Option[BaseSpecification] = None
  /** set the parent specification of this one */
  def setParent(s: BaseSpecification): this.type = { parentSpecification = Some(s); this }
  /** @return all the parent specifications of this specification, starting with the most immediate parent */
  def parentSpecifications: List[BaseSpecification] = {
    parentSpecification.map(List(_)).getOrElse(Nil) ::: parentSpecification.map(_.parentSpecifications).getOrElse(Nil)   
  } 
  /** this declares that a specification is composed of other specifications */
  def isSpecifiedBy(specifications: LazyParameter[Specification]*) = {
    addToName(" is specified by")
    include(specifications:_*)
  }
  /** alias for isSpecifiedBy */
  def areSpecifiedBy(specifications: LazyParameter[Specification]*) = {
    addToName(" are specified by")
    include(specifications:_*)
  }
  private def addToName(s: String) {
    this.description = this.name + s
  }
  /**
   * include a list of specifications inside this one
   */
  def include(specifications: LazyParameter[Specification]*) = {
    val toInclude = specifications.toStream.filter((s: LazyParameter[Specification]) => !(s.getValue() eq this) && !s.getValue().contains(this)).
                    map { s => s.getValue().setParent(this); s.getValue() }
    subSpecifications = subSpecifications ++ toInclude 
  }
  /** @return recursively all the systems included in this specification */
  def allSystems: List[Sus] = {
    systems ::: subSpecifications.flatMap(_.allSystems)
  }
  /** @return recursively all the examples included in this specification */
  def allExamples: List[Examples] = {
    systems.flatMap(_.allExamples) ::: subSpecifications.flatMap(_.allExamples)
  }
  /** @return true if it contains the specification recursively */
  def contains(s: Any): Boolean = {
    subSpecifications.contains(s) || subSpecifications.exists(_.contains(s))
  }
  /** @return the example corresponding to a given Tree path, searching in the incl */
  def getExample(path: TreePath): Option[Examples] = {
    path match {
      case TreePath(0 :: i :: rest) if systems.size > i => 
        systems(i).getExample(TreePath(rest))
      case _ => None
    }
  }
  /**
   * implicit definition allowing to declare a new example described by a string <code>desc</code><br>
   * Usage: <code>"return 0 when asked for (0+0)" in {...}</code><br>
   * Alternatively, it could be created with:
   * <code>forExample("return 0 when asked for (0+0)").in {...}</code>
   */
  implicit def specifyExample(desc: String): ExampleSpecification = {
    val example = exampleContainer.createExample(desc)
    if (sequential) example.setSequential()
    new ExampleSpecification(example)
  }
  class ExampleSpecification(val example: Example) {
    def specifies[T](expectations: =>T) = example.specifies(expectations)
    def in[T](expectations: =>T)(implicit m: scala.reflect.ClassManifest[T]) = example.in(expectations)(m)
    def >>[T](expectations: =>T)(implicit m: scala.reflect.ClassManifest[T]) = example.>>(expectations)(m)
  }
  def forExample(desc: String): Example = {
    specifyExample(desc).example
  }
  /**
   * Create an anonymous example, giving it a number depending on the existing created examples/
   */
  def forExample: Example = {
    forExample("example " + (exampleContainer.exampleList.size + 1))
  }
  /**
   * Return the example being currently executed if any
   */
  def lastExample: Option[Examples] = {
    current match {
      case Some(s: Sus) => None
      case Some(e: Example) => Some(e)
      case None => None
    }
  }
  /**
   * utility method to track the last example list being currently defined.<br>
   * It is either the current sus (one  gets created with specify if there's not any) or
   * the current example
   */
  protected[specification] def exampleContainer: Examples = {
    current.getOrElse {
      setCurrent(Some(specify))
      current.get
    }
  }
  /** the beforeAllSystems function will be invoked before all systems */
  var beforeSpec: Option[() => Any] = None
  /** the afterAllSystems function will be invoked after all systems */
  var afterSpec: Option[() => Any] = None
  /** if this variable is true then the doBeforeSpec block is not executed */
  private[specification] var beforeSpecHasBeenExecuted = false
  /** if this variable is true then the doBeforeSpec block is not executed and the example execution must fail */
  /** failure which may occur during the clean up of the spec */
  private[specification] var beforeSpecFailure: Property[SpecFailureException] = Property()
  /** failure which may occur during the clean up of the spec */
  private[specification] var afterSpecFailure: Property[SpecFailureException] = Property()
  /** if this variable is true then the doAfterSpec block is not executed */
  private[specification] var afterSpecHasBeenExecuted = false
  /** return true if no examples have been executed in this spec */
  private[specification] def isBeforeAllExamples = !beforeSpecHasBeenExecuted
  /** return true if this example is the last one of the spec */
  private[specs] def isTheLastExample(ex: Examples): Boolean = (!ex.hasSubExamples || ex.exampleList.isEmpty) && isTheLastExample(systems, ex)
  
  private def isTheLastExample(parents: List[Examples], ex: Examples): Boolean = {
    !parents.isEmpty && 
    !parents.last.exampleList.isEmpty && 
     (parents.last.exampleList.last == ex || isTheLastExample(parents.last.exampleList, ex))
  }
  /**
   * override the beforeExample method to execute actions before the
   * first example of the first sus
   */
  override def beforeExample(ex: Examples) = {
    beforeSpecFailure.foreach(throw _)
    if (!executeOneExampleOnly && isBeforeAllExamples) {
      executeSpecAction(beforeSpec, beforeSpecFailure, new BeforeSpecFailureException(_:FailureException))
      beforeSpecHasBeenExecuted = true
    }
  }
  /**
   * override the afterExample method to execute actions after the
   * last example of the last sus
   */
  override def afterExample(ex: Examples) = {
    afterSpecFailure.foreach(throw _)
    super.afterExample(ex)
  }
  /**
   * execute the afterSpec actions
   */
  private[specs] def executeAfterSpec {
    if (beforeSpecHasBeenExecuted && !afterSpecHasBeenExecuted) {
	    afterSpecHasBeenExecuted = true 
	    try {
        executeSpecAction(afterSpec, afterSpecFailure, new AfterSpecFailureException(_:FailureException))
      } catch {
        case _ => // ignore
      }
	  }
  }
  /**
   * execute the before or after specification action.
   * 
   * @param action the action to execute (possibly none)
   * @param specFailure a Property storing the possible FailureException created during the execution of the action
   *        this can happen if there are expectations in the action
   * @exceptionWrapper wrapper to create a specific action type around the failure which occurred during the action
   */
  private[specification] def executeSpecAction(action: Option[() => Any], 
                                               specFailure: Property[SpecFailureException],
                                               exceptionWrapper: FailureException => SpecFailureException) {
    setTemporarily(isSequential, true, (b:Boolean) => sequential = b,
                   executeOneExampleOnly, true, (b:Boolean) => executeOneExampleOnly = b,
                   expectationsListener, new DefaultExampleExpectationsListener {}, (e:ExampleExpectationsListener) => expectationsListener = e) {
      try {
        action.map { act => act() }
      } catch {
        case e: FailureException => specFailure(exceptionWrapper(e))
        case other => throw other 
    } finally {
        specFailure.foreach(throw _)
      }
    }
  }
  /** 
   * this variable commands if the specification has been instantiated to execute one example only, 
   * in order to execute it in isolation 
   */
  private[specification] var executeOneExampleOnly = false
  /**
   * Syntactic sugar for examples sharing between systems under test.<p>
   * Usage: <code>
   *   "A stack below full capacity" should {
   *    behave like "A non-empty stack below full capacity"
   *    ...
   * </code>
   * In this example we suppose that there is a system under specification with the same name previously defined.
   * Otherwise, an Exception would be thrown, causing the specification failure at construction time.
   */
   object behave {
    def like(o: =>Sus): Examples = {
      val other = o
      val behaveLike: Example = forExample("behave like " + other.description.uncapitalize)
      def originalExpectationsListener: Option[ExampleExpectationsListener] = other.parent match {
        case Some(p: ExampleExpectationsListener) => Some(p)
        case _ => None
      }

      behaveLike.in {
        originalExpectationsListener.map(_.expectationsListener = outer)
        other.prepareExecutionContextFrom(behaveLike)
        other.execution.map(_.execute)
        behaveLike.copyExecutionResults(other)
        other.resetForExecution
        other.exampleList = Nil
        behaveLike
      }
      behaveLike
    }
    def like(susName: String): Examples = outer.systems.find(_.description == susName) match {
      case Some(sus) => this.like(sus)
      case None => throw new Exception(q(susName) + " is not specified in " + outer.name + 
                                         outer.systems.map(_.description).mkString(" (available sus are: ", ", ", ")"))
    }
  }
  /** set an example as the current example for this lifecycle and its parent */
  override private[specs] def setCurrent(ex: Option[Examples]): Unit = {
    expectationsListener match {
      case l: LifeCycle if (l != this) => l.setCurrent(ex)
      case _ => ()
    }
    super.setCurrent(ex)
  }
  /** @return the current example */
  override private[specs] def current: Option[Examples] = {
    expectationsListener match {
      case l: LifeCycle if (l != this) => l.current
      case _ => super.current
    }
  }

  /** @return the first level examples number (i.e. without subexamples) */
  def firstLevelExamplesNb: Int = subSpecifications.foldLeft(0)(_+_.firstLevelExamplesNb) + systems.foldLeft(0)(_+_.examples.size)
  /** @return the failures of each sus */
  def failures: List[FailureException] = subSpecifications.flatMap(_.failures) ::: systems.flatMap(_.failures) ::: afterSpecFailure.toList
  /** @return the skipped of each sus */
  def skipped: List[SkippedException] = subSpecifications.flatMap{_.skipped} ::: systems.flatMap(_.skipped)
  /** @return the errors of each sus */
  def errors: List[Throwable] = subSpecifications.flatMap(_.errors) ::: systems.flatMap(_.errors)
  /** @return all the examples with no errors, failures or skip messages */
  def successes: List[Example] = subSpecifications.flatMap(_.successes) ::: systems.flatMap(_.successes)
  /** @return all the examples */
  def examples: List[Example] = subSpecifications.flatMap(_.examples) ::: systems.flatMap(_.examples)
  /** @return the total number of expectations for each sus */
  def expectationsNb: Int = subSpecifications.foldLeft(0)(_ + _.expectationsNb) + systems.foldLeft(0)(_ + _.expectationsNb)
  /** @return true if there are failures or errors */
  def isFailing: Boolean = !this.failures.isEmpty || !this.errors.isEmpty
  /** reset in order to be able to run the examples again */
  def resetForExecution: this.type = {
    subSpecifications.foreach(_.resetForExecution)
    systems.foreach(_.resetForExecution)
    this
  }
  /** Declare the subspecifications and systems as components to be tagged when the specification is tagged */
  override def taggedComponents: List[Tagged] = this.systems.toList ::: this.subSpecifications 
  /** @return the name of the specification */
  override def toString = name
  /** make sure that the execution is with shared variables when it is sequential */
  override def setSequentialIs(b: Boolean) = {
    super.shareVariablesIs(b)
    super.setSequentialIs(b)
  }
  override def shareVariablesIs(b: Boolean) = {
    if (!b) super.setSequentialIs(false)
    super.shareVariablesIs(b)
  }
 
}
trait ComposedSpecifications extends LazyParameters { this: BaseSpecification =>
/**
   * implicit definition allowing to declare a composition inside the current specification:
   * <code>"A complex specification".isSpecifiedBy(spec1, spec2)</code>
   * It changes the name of this specification with the parameter
   */
  implicit def declare(newName: String): ComposedSpecification = { 
    name = newName
    new ComposedSpecification(this) 
  }
  class ComposedSpecification(s: BaseSpecification) {
    def isSpecifiedBy(specifications: LazyParameter[Specification]*) = s.isSpecifiedBy(specifications:_*)
    def areSpecifiedBy(specifications: LazyParameter[Specification]*) = s.areSpecifiedBy(specifications:_*)
    def include(specifications: LazyParameter[Specification]*) = s.include(specifications:_*)
  }
}
abstract class SpecFailureException(message: String) extends FailureException(message)
class BeforeSpecFailureException(e: FailureException) extends SpecFailureException("Before specification:\n" + e.getMessage) {
  this.setAs(e)
}
class AfterSpecFailureException(e: FailureException) extends SpecFailureException("After specification:\n" + e.getMessage) {
  this.setAs(e)
}
