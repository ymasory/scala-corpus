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

/**
 * Exception class that encapsulates information about the stack depth at which the line of code that failed resides,
 * so that information can be presented to the user that makes it quick to find the failing line of code. (In other
 * words, the user need not scan through the stack trace to find the correct filename and line number of the problem code.)
 * Having a stack depth is more useful in a testing environment in which test failures are implemented as
 * thrown exceptions, as is the case in ScalaTest's built-in suite traits.
 *
 * @param message an optional detail message for this <code>StackDepthException</code>.
 * @param cause an optional cause, the <code>Throwable</code> that caused this <code>StackDepthException</code> to be thrown.
 * @param failedCodeStackDepth the depth in the stack trace of this exception at which the line of test code that failed resides.
 *
 * @throws NullPointerException if <code>message</code> is <code>null</code>, or <code>Some(null)</code>.
 * @throws NullPointerException if <code>cause</code> is <code>null</code>, or <code>Some(null)</code>.
 *
 * @author Bill Venners
 */
abstract class StackDepthException(val message: Option[String], val cause: Option[Throwable], val failedCodeStackDepth: Int)
    extends RuntimeException(if (message.isDefined) message.get else "", if (cause.isDefined) cause.get else null) with StackDepth {
  
  if (message == null) throw new NullPointerException("message was null")
  message match {
    case Some(null) => throw new NullPointerException("message was a Some(null)")
    case _ =>
  }

  if (cause == null) throw new NullPointerException("cause was null")
  cause match {
    case Some(null) => throw new NullPointerException("cause was a Some(null)")
    case _ =>
  }

  /*
  * Throws <code>IllegalStateException</code>, because <code>StackDepthException</code>s are
  * always initialized with a cause passed to the constructor of superclass <code>
  */
  override final def initCause(throwable: Throwable): Throwable = { throw new IllegalStateException }
}

/*
For check methods in Checkers, passed fileName will be "Checkers.scala" and
passed methodName will be "check":

0 org.scalatest.prop.Checkers$class.check(Checkers.scala:194)
1 org.scalatest.ShouldContainElementSpec.check(ShouldContainElementSpec.scala:23)
2 org.scalatest.prop.Checkers$class.check(Checkers.scala:205)
3 org.scalatest.ShouldContainElementSpec.check(ShouldContainElementSpec.scala:23)
4 org.scalatest.prop.Checkers$class.check(Checkers.scala:96)
5 org.scalatest.ShouldContainElementSpec.check(ShouldContainElementSpec.scala:23)
6 org.scalatest.ShouldContainElementSpec$$anonfun$1$$anonfun$apply$1$$anonfun$apply$28.apply(ShouldContainElementSpec.scala:80)

For detection of a duplicate test name in Spec, passed fileName will be "Spec.scala" and
passed methodName will be "it":

0 org.scalatest.Spec$class.registerExample(Spec.scala:682)
1 org.scalatest.Spec$class.it(Spec.scala:712)
2 org.scalatest.ShouldContainElementSpec.it(ShouldContainElementSpec.scala:23)
3 org.scalatest.Spec$class.it(Spec.scala:735)
4 org.scalatest.ShouldContainElementSpec.it(ShouldContainElementSpec.scala:23)
5 org.scalatest.ShouldContainElementSpec$$anonfun$1$$anonfun$apply$167.apply(ShouldContainElementSpec.scala:1092) 

For detection of a duplicate test name in FunSuite, passed fileName will be "FunSuite.scala" and
passed methodName will be "test":

0 org.scalatest.FunSuite$class.test(FunSuite.scala:592)
1 org.scalatest.SpecSuite.test(SpecSuite.scala:18)
2 org.scalatest.SpecSuite.<init>(SpecSuite.scala:42) 

For detection of an it inside an it in Spec, passed fileName will be "Spec.scala" and
passed methodName will be "it":

0 org.scalatest.Spec$class.it(Spec.scala:745)
1 org.scalatest.ShouldBehaveLikeSpec.it(ShouldBehaveLikeSpec.scala:23)
2 org.scalatest.ShouldBehaveLikeSpec$$anonfun$1.apply(ShouldBehaveLikeSpec.scala:26) 

For detection of a describe inside an it in Spec, passed fileName will be "Spec.scala" and
passed methodName will be "describe":

0 org.scalatest.Spec$class.describe(Spec.scala:804)
1 org.scalatest.ShouldBehaveLikeSpec.describe(ShouldBehaveLikeSpec.scala:23)
2 org.scalatest.ShouldBehaveLikeSpec$$anonfun$1.apply(ShouldBehaveLikeSpec.scala:26) 

For detection of an ignore inside an it in Spec, passed fileName will be "Spec.scala" and
passed methodName will be "ignore":

0 org.scalatest.Spec$class.ignore(Spec.scala:792)
1 org.scalatest.ShouldBehaveLikeSpec.ignore(ShouldBehaveLikeSpec.scala:23)
2 org.scalatest.ShouldBehaveLikeSpec$$anonfun$1.apply(ShouldBehaveLikeSpec.scala:26) 

For detection of a test inside a test in FunSuite, passed fileName will be "FunSuite.scala" and
passed methodName will be "test":

0 org.scalatest.FunSuite$class.test(FunSuite.scala:591)
1 org.scalatest.Q36Suite.test(ShouldBehaveLikeSpec.scala:23)
2 org.scalatest.Q36Suite$$anonfun$1.apply(ShouldBehaveLikeSpec.scala:25)

For detection of an ignore inside a test in FunSuite, passed fileName will be "FunSuite.scala" and
passed methodName will be "ignore":

0 org.scalatest.FunSuite$class.ignore(FunSuite.scala:624)
1 org.scalatest.Q36Suite.ignore(ShouldBehaveLikeSpec.scala:23)
2 org.scalatest.Q36Suite$$anonfun$1.apply(ShouldBehaveLikeSpec.scala:25)
*/
private[scalatest] object StackDepthExceptionHelper {

  def getStackDepth(fileName: String, methodName: String): Int = {

    val temp = new RuntimeException
    val stackTraceList = temp.getStackTrace.toList.tail // drop the first one, which is this getStackDepth method

    val fileNameIsCheckersDotScalaList: List[Boolean] =
      for (element <- stackTraceList) yield
        element.getFileName == fileName // such as "Checkers.scala"

    val methodNameIsCheckList: List[Boolean] =
      for (element <- stackTraceList) yield
        element.getMethodName == methodName // such as "check"

    // For element 0, the previous file name was not Checkers.scala, because there is no previous
    // one, so you start with false. For element 1, it depends on whether element 0 of the stack trace
    // had file name Checkers.scala, and so forth.
    val previousFileNameIsCheckersDotScalaList: List[Boolean] = false :: (fileNameIsCheckersDotScalaList.dropRight(1))

    // Zip these two related lists together. They now have two boolean values together, when both
    // are true, that's a stack trace element that should be included in the stack depth. In the 
    val zipped1 = methodNameIsCheckList zip previousFileNameIsCheckersDotScalaList
    val methodNameIsCheckAndPreviousFileNameIsCheckersDotScalaList: List[Boolean] =
      for ((methodNameIsCheck, previousFileNameIsCheckersDotScala) <- zipped1) yield
        methodNameIsCheck && previousFileNameIsCheckersDotScala

    // Zip the two lists together, that when one or the other is true is an include.
    val zipped2 = fileNameIsCheckersDotScalaList zip methodNameIsCheckAndPreviousFileNameIsCheckersDotScalaList
    val includeInStackDepthList: List[Boolean] =
      for ((fileNameIsCheckersDotScala, methodNameIsCheckAndPreviousFileNameIsCheckersDotScala) <- zipped2) yield
        fileNameIsCheckersDotScala || methodNameIsCheckAndPreviousFileNameIsCheckersDotScala

    includeInStackDepthList.takeWhile(include => include).length
  }
}
