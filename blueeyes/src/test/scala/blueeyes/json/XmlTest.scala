/*
 * Copyright 2009-2010 WorldWide Conferencing, LLC
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

package blueeyes {
package json {

import _root_.org.scalacheck._
import _root_.org.scalacheck.Prop.forAll
import _root_.org.specs.Specification
import _root_.org.specs.runner.{Runner, JUnit}
import _root_.org.specs.ScalaCheck

class XmlTest extends Runner(XmlSpec) with JUnit
object XmlSpec extends Specification with NodeGen with JValueGen with ScalaCheck {
  import Xml._
  import JsonAST._
  import Printer.compact
  import JsonParser.parse
  import scala.xml.Node

  "Valid XML can be converted to JSON and back (symmetric op)" in {
    val conversion = (xml: Node) => { toXml(toJson(xml)).text == xml.text }
    forAll(conversion) must pass
  }

  "JSON can be converted to XML, and back to valid JSON (non symmetric op)" in {
    val conversion = (json: JValue) => { parse(compact(render(toJson(toXml(json))))); true }
    forAll(conversion) must pass
  }

  implicit def arbXml: Arbitrary[Node] = Arbitrary(genXml)
  implicit def arbJValue: Arbitrary[JValue] = Arbitrary(genObject)
}

}
}
