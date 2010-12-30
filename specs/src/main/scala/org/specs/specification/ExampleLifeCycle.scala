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
import org.specs.execute.{ FailureException, SkippedException }
import org.specs.util.Configuration
import org.specs.util.ExtendedThrowable._

/**
 * This trait models the execution cycle of an example.
 * 
 * LifeCycles can be chained with a "parent" relationship, where the parent LifeCycle of an example is his 
 * enclosing sus and the parent LifeCycle of a Sus is its enclosing specification.
 * 
 * Since a Specification mixes in this trait, some methods can be overriden to provide more specialized behaviour:
 * <ul>
 * <li>before(/after)Example: code to execute before(/after) the example</li>
 * <li>before(/after)Expectations: code to execute before(/after) the example expectations. The difference with beforeExample is that 
 * any FailureException thrown by this method will be counted as an example failure (whereas it is counted as an exception in beforeExample)</li>
 * <li>executeExpectations: execute the example expectations</li>
 * <li>executeExample: strategy for executing the example. The default strategy is implemented in the ExampleLifeCycle trait. 
 * In this trait, we check if the example is the first one to execute. If so, it is executed right away. If not, the execution is 
 * delegated to the parent LifeCycle which ultimately is the SpecificationExecutor trait which executes the example in
 * isolation from other examples by executing it in a clone of the Specification.
 * </li>
 * </ul>
 *  
 */
trait LifeCycle extends SequentialExecution {
  /** lifecycles can be chained via parent-child relationships */
  private[specs] var parent: Option[LifeCycle] = None
  /** 
   * current list of examples defining the context of this execution. Every example created during by the executeExpectations method
   * will be attached to this list
   */
  private[specs] var currentExample: Option[Examples] = None
  /** a predicate which will decide if an example must be re-executed */
  private[specs] var untilPredicate: Option[() => Boolean] = None
  /** if this variable is true then the doFirst block is not executed and the example execution must fail */
  private[specification] var beforeSystemFailure: Option[FailureException] = None
  /** execute a block of code with a specific list of examples as the container to use for examples created by the block */
  private[specs] def withCurrent(ex: Examples)(a: => Any) = {
    val c = current.orElse(parent.flatMap(_.current))
    setCurrent(Some(ex))
    try { a } finally {
      setCurrent(c)
    }
  }
  /** set an example as the current example for this lifecycle and its parent */
  private[specs] def setCurrent(ex: Option[Examples]): Unit = {
    currentExample = ex
    parent.map(_.setCurrent(ex))
  }
  /** @return the current example */
  private[specs] def current: Option[Examples] = currentExample
  /** @return true if there is an until predicate, and if it is true */
  def until: Boolean = parent.map(_.until).getOrElse(true) && untilPredicate.getOrElse(() => true)()
  /** define the actions to be done before an example is executed */
  def beforeExample(ex: Examples) = {}
  /** define the actions to be done after an example has been executed */
  def afterExample(ex: Examples) = {}
  /** define actions to be done just before the example expectations, as if they were part of the example */
  def beforeExpectations(ex: Examples): Unit = {
    parent.map(_.beforeExpectations(ex))
  }
  /** define actions to be done just after the example expectations, as if they were part of the example */
  def afterExpectations(ex: Examples): Unit = parent.map(_.afterExpectations(ex))
  /** define how to execute the example expectations */
  def executeExpectations(ex: Examples, t: =>Any): Any = parent.map(_.executeExpectations(ex, t)).getOrElse(t)
  /** define how to execute the example itself. This method has to be overriden by subtraits */
  def executeExample(ex: Examples): this.type = this
}
/**
 * Implementation of a LifeCycle backed up by an ExampleStructure which can
 * be added any subexample created during the Example execution.
 * 
 * It will also check that the executed example contains expectations,
 * otherwise will mark the example as "PENDING"
 */
trait ExampleLifeCycle extends LifeCycle with ExampleStructure {
  /** 
   * object containing the method for executing the example expectations, including when and how
   * the LifeCycle methods should be called.
   */
  private[specs] var execution: Option[ExampleExecution] = None
  /** @return true if the execution has been executed */
  private[specs] def executed = execution.map(_.executed).getOrElse(true)
  /** abstract method (defined in Example) executing the example itself */
  def executeThis: Unit
  /** 
   * executing the expectations of an example contained by this LifeCycle, 
   * skipping the example if it doesn't contain any expectations 
   */
  override def afterExpectations(ex: Examples) = {
    super.afterExpectations(ex)
    skipIfNoExpectations()
  }
  /** 
   * execute one sub example either right away if this is the first example
   * of this list of example, otherwise the execution is delegated to the parent lifecycle
   * for isolated execution
   */
  override def executeExample(ex: Examples): this.type = { 
    parent.map(_.executeExample(ex)) // forward the execution strategy to the parent 
    this
  }
  /**
   * copy the execution results from another example.
   * This method is used when an example has been executed in isolation in another spec.
   */
  def copyExecutionResults(other: Examples) {
    copyFrom(other)
    execution.map(_.executed = true)
  }
  /**
   * @throw a skipped exception if there are no expectations in that example. This behaviour can be overriden by setting the
   * "examplesWithoutExpectationsMustBePending" property in the Configuration object
   */
  protected def skipIfNoExpectations() = {
    if (thisExpectationsNumber == 0 && 
        exampleList.isEmpty && thisSkipped.isEmpty && thisFailures.isEmpty && thisErrors.isEmpty && 
        Configuration.config.examplesWithoutExpectationsMustBePending)
      throw new SkippedException("PENDING: not yet implemented").removeTracesAsFarAsNameMatches("(specification.Example|LiterateSpecification)")
  }
  /** reset in order to be able to run the example again. This is only used for testing */
  override def resetForExecution: this.type = {
    super.resetForExecution
    execution.map(_.resetForExecution)
    exampleList.foreach(_.resetForExecution)
    this
  }
}
/** Default LifeCycle with no actions before or after. */
object DefaultLifeCycle extends Example("default life cycle")

/** This trait defines if the examples must be executed as soon as they are defined */
trait SequentialExecution {
  /** this variable defines if examples should be executed as soon as defined */
  private[specs] protected var sequential = false
  /** @return true if if examples should be executed as soon as defined  */
  def isSequential = sequential
  /** examples should be executed as soon as defined */
  def setSequential() = setSequentialIs(true)
  /** examples should not be executed as soon as defined */
  def setNotSequential() = setSequentialIs(false)
  /** setter for is sequential */
  def setSequentialIs(b: Boolean) = sequential = b

}

/**
 * This class encapsulates the execution of an example.
 * It will check if the example is tagged for execution ("accepted") and it will execute the expectations as
 * long as the untilr predicate is not satisfied (if there is one)
 */
class ExampleExecution(var example: Examples, var expectations: Examples => Any) {
  /** function containing the expectations to be run */
  private var toRun: () => Any = () => {
    if (example.isAccepted) {
      execution()
      while (!example.parent.map(_.until).getOrElse(true)) execution()
    } else
      example.addSkipped(new SkippedException("not tagged for execution"))
  }

  /** flag used to memorize if the example has already been executed once. In that case, it will not be re-executed */
  private[specification] var executed = false
  /** 
   * execution of the example expectations. The example lifecycle methods are used here:<ul>
   * <li>beforeExample: add an error and skip the rest if an exception is thrown</li>
   * <li>beforeExpectations</li>
   * <li>executeExpectations</li>
   * <li>afterExpectations</li>
   * <li>afterExample: add an error if an exception is thrown</li>
   * <ul/>
   */
  val execution = () => {
    var failed = false
    // try the "before" methods. If there is an exception, add an error and return the current example
    try { example.beforeExample(example) } catch {
      case f: FailureException => {
        example.addFailure(f)
        failed = true
      }
      case s: SkippedException => {
        example.addSkipped(s)
        failed = true
      }
      case t: Throwable => {
        example.addError(t)
        failed = true
      }
    }
    // execute the <code>expectations</code> parameter. If it contains expectations with matchers they will be automatically executed
    try {
      if (!failed) {
        example.beforeExpectations(example)
        example.executeExpectations(example, expectations(example))
        example.afterExpectations(example)
      }
    } catch {
      // failed expectations will launch a FailureException
      // skipped expectations will launch a SkippedException
      case f: FailureException => example.addFailure(f)
      case s: SkippedException => example.addSkipped(s)
      case t: Throwable => example.addError(t)
      }
      // try the "after" methods. If there is an exception, add an error and return the current example
      try {
        if (!failed)
          example.afterExample(example)
      } catch { 
        case f: FailureException => {
          example.addFailure(f)
          failed = true
        }
        case s: SkippedException => {
          example.addSkipped(s)
          failed = true
        }
        case t: Throwable => {
          example.addError(t)
          failed = true
        }
      }
      example
  }
  /** execute the example, setting a flag to make sure that it is only executed once */
  def execute = {
    if (!executed) {
      toRun()
      executed = true
    }
  }
  /** reset the execution so that a new call to execute will indeed execute the expectations again */
  def resetForExecution = executed = false
}
