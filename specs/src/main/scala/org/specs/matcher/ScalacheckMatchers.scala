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
package org.specs.matcher
import org.scalacheck.{Gen, Prop, Arg, Test, Arbitrary, Shrink}
import org.scalacheck.util.StdRand
import org.scalacheck.Prop._
import org.scalacheck.Test.{Status, Params, Proved, Passed, Failed, Exhausted, GenException, PropException, Result}
import org.scalacheck.Pretty._
import org.scalacheck.Pretty
import org.scalacheck.ConsoleReporter._
import scala.collection.Map
import org.specs.io.ConsoleOutput
import org.specs.matcher._
import org.specs.util.ExtendedFunctions._
import org.specs.matcher.MatcherUtils.q
import org.specs.execute._
import org.specs.specification._
/**
 * The <code>ScalaCheckMatchers</code> trait provides matchers which allow to
 * assess properties multiple times with generated data.
 * @see the <a href="http://code.google.com/p/scalacheck/">ScalaCheck project</a>
 */
trait ScalaCheckMatchers extends ConsoleOutput with ScalaCheckFunctions with ScalaCheckParameters with SuccessValues with ExpectationsListener { outer =>

  /**
   * This implicit value is useful to transform the SuccessValue returned by matchers to properties.
   * More specifically, this allows to write expectations as properties:
   * (1 + 1) must_== 2 will return a SuccessValue if it is not throwing a FailureException.
   * That success value can then be considered as a property in an example:
   *
   * <code>{ (1 + 1) must_== 2 } must pass</code>
   *
   * @see Expectable
   */
  implicit val successValueToProp: SuccessValue => Prop = (s: SuccessValue) => Prop.passed

   /**
    * default parameters. Uses ScalaCheck default values and doesn't print anything to the console
    */
   implicit def defaultParameters = new Parameters(setParams(Nil))

   /** default parameters to display pretty messages */		   
   val defaultPrettyParams = Pretty.defaultParams
   /**
    * Matches ok if the <code>function T => Boolean</code> returns <code>true</code> for any generated value<br>
    * Usage: <code>function must pass(generated_values)</code><br>
    * @param params are the given by the implicit default parameters of ScalaCheck
    */
   def pass[T](g: Gen[T])(implicit params: Parameters) = new Matcher[T => Boolean]() {
     def apply(f: => (T => Boolean)) = checkFunction(g)(f)(params)
   }
   implicit def booleanFunctionToPropFunction[T](f: T => Boolean): T => Prop = (t: T) => {
     if (f(t)) proved else falsified
   }
   /** 
    * This implicit definition and associated ForAll class allows to write
    * { function }.forAll must pass
    */
   implicit def toProp[T](f: T => Boolean): ForAll[T] = new ForAll(f)
   class ForAll[T](f: T => Boolean) {
     def forAll(implicit a: Arbitrary[T], s: Shrink[T]) = Prop.forAll(f)
   }

   /**
    * Matches ok if the <code>function T => Prop</code> returns a<code>true</code> Property for any generated value<br>
    * Usage: <code>generated_values must pass(function)</code>
    */
   def pass[T](f: T => Prop)(implicit params: Parameters) = new Matcher[Gen[T]](){
     def apply(g: => Gen[T]) = checkProperty(forAllProp(g)(f))(params)
   }
   /**
    * Matches ok if the <code>function T => Boolean</code> returns <code>true</code> for any generated value<br>
    * Usage: <code>generated_values must pass(function)</code>
    */
   def pass[T](f: T => SuccessValue)(implicit params: Parameters) = new GenMatcher[T](f)(params)
   /**
    * Matches ok if the partial function<code> { case t => exp }</code> returns <code>true</code> for any generated value<br>
    * exp is an expression returning a SuccessValue, so that any specs expectation can be used here, like a must_== b
    * Usage: <code>generated_values must validate(partial function)</code>
    */
   def validate[T](f: Function[T, SuccessValue])(implicit params: Parameters) = new GenMatcher[T](t => f.applySafely(t).getOrElse(false))(params)

   /** transforms a boolean to a SuccessValue so that Partial functions returning booleans can be accepted by the validate matcher */
   implicit def booleanToSuccessValue(b: => Boolean) = new SuccessValue { if (!b) throw new FailureException("false") }

   /** adds a validates method to a generator so that gen validates partialFunction can be written */
   implicit def aGen[T](g: Gen[T]) = new AGen(g)
   class AGen[T](g: Gen[T]) {
     def validates(f: Function[T, SuccessValue])(implicit params: Parameters) = outer.validate(f)(params).apply(g)
   }
   /** 
    * workaround class used to avoid an ambiguous definition of the pass method with
    * f: T => Boolean and f: T => SuccessValue 
    */
   class GenMatcher[T](f: T => SuccessValue)(implicit params: Parameters) extends Matcher[Gen[T]] {
     def apply(g: => Gen[T]) = checkFunction(g){(t: T) => f(t); true}(params)
   }
   /**
    * Matches ok if the <code>property</code> is proved for any generated value<br>
    * Usage: <code>generated_values must pass(property)</code>
    */
   def pass[T](prop: Prop)(implicit params: Parameters) = new Matcher[Gen[T]](){
     def apply(g: => Gen[T]) = checkProperty(forAllProp(g)(a => prop))(params)
   }

   /**
    * Matches ok if the <code>property</code> is proved for any generated value<br>
    * Usage: <code>property must pass</code>
    */
    def pass(implicit params: Parameters) = new Matcher[Prop](){
      def apply(p: => Prop) = checkProperty(p)(params)
    }

    private [matcher] def checkFunction[T](g: Gen[T])(f: T => Boolean)(p: Parameters) = {
      // create a scalacheck property which states that the function must return true
      // for each generated value
      val prop = forAllProp(g)(a => if (f(a)) proved else falsified)
      checkProperty(prop)(p)
   }
   /**
    * checks if the property is true for each generated value, and with the specified
    * generation parameters <code>p</code>. <code>p</code> is transformed into a scalacheck parameters
    * and indicates if the generation should be verbose or not
    */
   private [matcher] def checkProperty(prop: Prop)(p: Parameters) = {
     checkScalaCheckProperty(prop)(Params(p(minTestsOk), p(maxDiscarded), p(minSize), p(maxSize), StdRand, p(workers), p(wrkSize)), p.verbose)
   }

  /**
   * checks if the property is true for each generated value, and with the specified
   * scalacheck parameters. If verbose is true, then print the results on the console
   */
  private [matcher] def checkScalaCheckProperty(prop: Prop)(params: Params, verbose: Boolean) = {
     // will print the result of each test if verbose = true
     def printResult(succeeded: Int, discarded: Int): Unit = {
       if (!verbose) return
       if (discarded == 0)
         printf("\rPassed %d tests", succeeded)
       else
         printf("\rPassed %d tests; %d discarded", succeeded, discarded)
       flush
     }

     // check the property
     def propToCheck = if (!shouldCountExpectations) prop else (prop && Prop.forAll((t: Boolean) => true.isExpectation))
     val results = checkProp(params, propToCheck, printResult)

     // display the final result if verbose = true
     if (verbose) {
       val s = prettyTestRes(results)(defaultPrettyParams)
       printf("\r%s %s%s\n", if (results.passed) "+" else "!", s, List.fill(70 - s.length)(" ").mkString(""))
     }

     results match {
       case Result(Proved(as), succeeded, discarded, _) => (true,  noCounterExample(succeeded), "A counter-example was found " + afterNTries(succeeded))
       case Result(Passed, succeeded, discarded, _) => (true,  noCounterExample(succeeded), "A counter-example was found " + afterNTries(succeeded))
       case r@Result(GenException(e), n, _, _) => (false, noCounterExample(n), prettyTestRes(r)(defaultPrettyParams))
       case r@Result(Exhausted, n, _, _)     => (false, noCounterExample(n), prettyTestRes(r)(defaultPrettyParams))
       case Result(Failed(args, labels), n, _, _) =>
         (false, noCounterExample(n), "A counter-example is "+counterExample(args)+" (" + afterNTries(n) + afterNShrinks(args) + ")" + failedLabels(labels))
       case Result(PropException(args, FailureException(ex), labels), n, _, _) =>
         (false, noCounterExample(n), "A counter-example is "+counterExample(args)+": " + ex + " ("+afterNTries(n)+")"+ failedLabels(labels))
       case r@Result(PropException(m, ex, labels), n, _, _) =>
         (false, noCounterExample(n), prettyTestRes(r)(defaultPrettyParams)+ failedLabels(labels))
     }
   }
   // depending on the result, return the appropriate success status and messages
   // the failure message indicates a counter-example to the property
   private [matcher] def noCounterExample(n: Int) = "The property passed without any counter-example " + afterNTries(n)
   private [matcher] def afterNTries(n: Int) = "after " + (if (n == 1) n + " try" else n + " tries")
   private [matcher] def afterNShrinks(args: List[Arg[_]]) = {
     if (args.forall(_.shrinks == 0))
       ""
     else
       args.map { arg =>
         if (arg.origArg != arg.arg)
           q(arg.origArg) +" -> " + q(arg.arg)
         else
           " = "
      }.mkString(" - shrinked (", ",", ")")
   }

   private [matcher] def counterExample(args: List[Arg[_]]) = {
     if (args.size == 1)
       args.map(a => if (a.arg == null) "null" else a.arg.toString).mkString("'", "", "'")
     else if (args.exists(_.arg.toString.isEmpty))
       args.map(_.arg).mkString("['", "', '", "']")
     else
       args.map(_.arg).mkString("[", ", ", "]")
   }
   private [matcher] def failedLabels(labels: Set[String]) = {
     if (labels.isEmpty)
       ""
     else
       labels.mkString("\nlabels of failing property: ", ", ", "\n")
   }

}
/**
 * This trait is used to facilitate testing by mocking ScalaCheck functionalities
 */
trait ScalaCheckFunctions {
  def checkProp(params: Params, prop: Prop, printResult: (Int, Int) => Unit) = Test.check(params, prop, printResult)
  def forAllProp[A,P](g: Gen[A])(f: A => Prop): Prop = Prop.forAll(g)(f)
}
/**
 * This trait provides generation parameters to use with the <code>ScalaCheckMatchers</code>
 */
trait ScalaCheckParameters {
  /**
   * Values which can be used as Symbol aliases to specify ScalaCheck parameters<br>
   * The naming is a bit different, in order to keep short names for frequent use cases<ul>
   *  <code><li>minTestsOk == minSuccessfulTests
   *  <li>maxDiscarded == maxDiscardedTests
   *  <li>minSize and maxSize keep their name <code><ul>
   */
  val (minSize, maxSize, maxDiscarded, minTestsOk, workers, wrkSize) = ('minSize, 'maxSize, 'maxDiscarded, 'minTestsOk, 'workers, 'wrkSize)

  /** This variable is used to track if we need to add an expectation each time a property is evaluated */
  private var countExpectations = true
  /** declare that an expectation should be added each time a property is evaluated (default) */
  def expectProperties() = { countExpectations = true; this }
  /** declare that no expectation should be added each time a property is evaluated */
  def dontExpectProperties() = { countExpectations = false; this }
  private [matcher] def shouldCountExpectations = countExpectations
  /**
   * Default values for ScalaCheck parameters
   */
  def defaultValues = Map(minTestsOk->100, maxDiscarded ->500, minSize->0, maxSize->100, workers->1, wrkSize->20)

  /**
   * This object is used to set parameters but nothing will be printed to the console<br>
   * Usage: <pre><code>
   * generated_values must pass { v =>
   *   property(v) mustBe ok
   * }(set(minTestsOk->15, maxDiscarded->20))</code></pre>
   */
  object set extends Parameters(setParams(Nil)) {
    def apply(p: (Symbol, Int)*) = new Parameters(setParams(p))
  }

  /**
   * Those parameters will print the result on the console and use the default settings, or specified parameters <br>
   * Usage: <pre><code>
   * generated_values must pass { v =
   *   property(v) mustBe ok
   * }(display) </code></pre>
   *
   *  or
   *
   *  generated_values must pass { v =>
   *    property(v) mustBe ok
   *  }(display(minTestsOk->15, maxDiscarded->20))</code></pre>
   */
  object display  extends Parameters(setParams(Nil)) {
    def apply(p: (Symbol, Int)*) = new Parameters(setParams(p)) { override def verbose = true }
    override def verbose = true
  }

  /**
   * This function transform the varargs parameters into a Map with default values
   * if some expected values are not provided by the user
   */
  def setParams(p: Seq[(Symbol, Int)]): Map[Symbol, Int] = {
    var params: Map[Symbol, Int] = new scala.collection.immutable.HashMap[Symbol, Int].withDefault(defaultValues)
    p foreach { pair: (Symbol, Int) =>
        //  this is a useful check in case of print(null) or set(null)
        if (pair == null || pair._1 == null)
          throw new RuntimeException("null values are not accepted in scalacheck parameters: " + q(pair))
        else {
          val (s, i) = pair
          params = params + Pair(s, i)
        }
    }
    params
  }
}
/**
 * This class is the base class for the display and set case classes.<br>
 * It contains a Map of generation parameters and indicates if the generation
 * must be verbose.
 */
case class Parameters(params: Map[Symbol, Int]) {
  def apply(s: Symbol) = params(s)
  def verbose = false
}
