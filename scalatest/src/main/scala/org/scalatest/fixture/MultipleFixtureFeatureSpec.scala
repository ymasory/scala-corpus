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

/**
 * A sister trait to <code>org.scalatest.FeatureSpec</code> that can pass multiple types of fixture objects into its tests.
 *
 * <p>
 * This trait behaves similarly to trait <code>org.scalatest.FeatureSpec</code>, except that tests may take a fixture object, and unlike
 * a <code>FixtureFeatureSpec</code>, different tests may take different types of fixtures. This trait extends <code>FixtureFeatureSpec</code>
 * and mixes in <code>ConfigMapFixture</code>, which defines the <code>Fixture</code> type to be the <code>configMap</code>'s
 * type (<code>Map[String, Any]</code>) and defines the <code>withFixture</code> method to simply pass the <code>configMap</code>
 * to the test function. To write tests that take fixtures of types other than <code>Fixture</code> (<em>i.e.</em>,
 * <code>Map[String, Any]</code>), you can define implicit conversions from a function of type <code>(</code>&lt;the fixture type&gt;<code>) =&gt; Unit</code>
 * to a function of type <code>(FixtureParam) =&gt; Unit</code>. Each implicit conversion method serves as the with-fixture method for that type.
 * </p>
 * 
 * <p>
 * Subclasses of this trait must, therefore, do two things differently from a plain old <code>org.scalatest.FeatureSpec</code>:
 * </p>
 * 
 * <ol>
 * <li>define implicit <code>with<em>&lt;type&gt;</em>Fixture</code> methods</li>
 * <li>write tests that take the different fixture types for which you've defined implicit conversion methods (You can also define tests that don't take a <code>Fixture</code>.)</li>
 * </ol>
 *
 * <p>
 * Here's an example that has two fixture types, <code>String</code> and <code>List[Int]</code>:
 * </p>
 * 
 * <pre>
 * import org.scalatest.fixture.MultipleFixtureFeatureSpec
 * 
 * class MyFeatureSpec extends MultipleFixtureFeatureSpec {
 * 
 *   // The "with-fixture" method for tests that take a String fixture
 *   implicit def withStringFixture(testFunction: String => Unit): FixtureParam => Unit =
 *     testFunction("howdy")
 * 
 *   // The "with-fixture" method for tests that take a List[Int] fixture
 *   implicit def withListFixture(testFunction: List[Int] => Unit): FixtureParam => Unit =
 *     configMap => testFunction(List(configMap.size))
 * 
 *   // A test that takes a String fixture
 *   scenario("takes a string fixture") { (s: String) =>
 *     assert(s === "howdy")
 *   }
 * 
 *   // A test that takes a List[Int] fixture
 *   scenario("takes a list fixture") { (list: List[Int]) =>
 *     assert(list.size === 1)
 *   }
 * 
 *   // A test that takes no fixture
 *   scenario("takes no fixture") { () =>
 *     assert(1 === 1)
 *   }
 * }
 * </pre>
 *
 * <p>
 * The first method in this class, <code>withStringFixture</code>, is the implicit conversion function for tests that take a fixture
 * of type <code>String</code>.  In this contrived example, the hard-coded string <code>"howdy"</code> is passed into the test:
 * </p>
 *
 * <pre>
 * implicit def withStringFixture(testFunction: String => Unit): FixtureParam => Unit =
 *   testFunction("howdy")
 * </pre>
 * 
 * <p>
 * Although the result type of this implicit conversion method is <code>FixtureParam => Unit</code>, if a fixture doesn't need anything
 * from the <code>configMap</code>, you can leave off the <code>configMap =></code> at the beginning of the result function. The
 * reason this works is that <code>MultipleFixtureFeatureSpec</code> inherits an implicit conversion from a by-name parameter to
 * <code>FixtureParam => Unit</code> from supertrait <code>FixtureFeatureSpec</code>. This implicit conversion is used to enable 
 * tests that take no fixture (such as the one named <code>takes no fixture</code> in this example) to be included in the
 * same class as tests that take type <code>Fixture</code>. That same implicit conversion is used here to allow you to leave off
 * the <code>configMap =></code> except when you actually need something from the <code>configMap</code>. By leaving it off, you
 * indicte to readers of the code that this fixture doesn't require anything from the <code>configMap</code>.
 * </p>
 *
 * <p>
 * The next method in this class, <code>withListFixture</code>, is the implicit conversion function for tests that take a
 * fixture of type <code>List[Int]</code>.  In this contrived example, a <code>List[Int]</code> that contains one element, the
 * size of the <code>configMap</code>, is passed to the test function. Because the fixture uses the <code>configMap</code>,
 * it has an explicit <code>configMap =></code> parameter at the beginning of the result. (Remember, the <code>Fixture</code>
 * type is defined to be <code>Map[String, Any]</code>, the type of the <code>configMap</code>, in supertrait <code>ConfigMapFixture</code>.
 * </p>
 *
 * <p>
 * Following the implicit conversion methods are the test declarations. One test is written to take the <code>String</code> fixture:
 * </p>
 *
 * <pre>
 * scenario("takes a string fixture") { (s: String) =>
 *   assert(s === "howdy")
 * }
 * </pre>
 *
 * <p>
 * What happens at compile time is that because the <code>Fixture</code> type is <code>Map[String, Any]</code>, the <code>test</code> method
 * should be passed a function from type <code>(Map[String, Any]) => Unit</code>, or using the type alias, <code>(FixtureParam) => Unit</code>. Passing
 * a function of type <code>String => Unit</code> as is attempted here is a type error. Thus the compiler will look around for an implicit
 * conversion that will fix the type error, and will find the <code>withStringFixture</code> method. Because this is the only implicit
 * conversion that fixes the type error, it will apply it, effectively generating this code:
 * </p>
 *
 * <pre>
 * // after the implicit withStringFixture method is applied by the compiler
 * scenario("takes a string fixture") {
 *   withStringFixture { (s: String) =>
 *     assert(s === "howdy")
 *   }
 * }
 * </pre>
 *
 * <p>
 * After passing the <code>(String) => Unit</code> function to <code>withStringFixture</code>, the result will be of
 * type <code>(FixtureParam) => Unit</code>, which the <code>test</code> method expects.
 * </p>
 *
 * <p>
 * The next test is written to take the <code>List[Int]</code> fixture:
 * </p>
 *
 * <pre>
 * scenario("takes a list fixture") { (list: List[Int]) =>
 *   assert(list.size === 1)
 * }
 * </pre>
 *
 * <p>
 * The compiler will apply the <code>withListFixture</code> implicit conversion in this case, effectively generating the following
 * code:
 * </p>
 *
 * <pre>
 * scenario("takes a list fixture") {
 *   withListFixture { (list: List[Int]) =>
 *     assert(list.size === 1)
 *   }
 * }
 * </pre>
 * 
 * <p>
 * Note that in a <code>FixtureFeatureSpec</code>, you need to specify the type of the fixture explicitly so the compiler knows
 * the type to convert from. So you must, for example, write:
 * </p>
 *
 * <pre>
 * scenario("takes a list fixture") { (list: List[Int]) =>
 *   assert(list.size === 1)
 * }
 * </pre>
 *
 * <p>
 * The following attempt will fail to compile:
 * </p>
 *
 * <pre>
 * // won't compile, because list is inferred to be of type FixtureParam
 * scenario("takes a list fixture") { list =>
 *   assert(list.size === 1)
 * }
 * </pre>
 *
 * @author Bill Venners
 */
trait MultipleFixtureFeatureSpec extends FixtureFeatureSpec with ConfigMapFixture
