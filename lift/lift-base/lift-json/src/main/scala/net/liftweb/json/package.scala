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

package net.liftweb {
package object json {
  import java.io.Reader
  import scala.text.Document

  type JValue   = JsonAST.JValue
  val  JNothing = JsonAST.JNothing
  val  JNull    = JsonAST.JNull
  type JString  = JsonAST.JString
  val  JString  = JsonAST.JString
  type JDouble  = JsonAST.JDouble
  val  JDouble  = JsonAST.JDouble
  type JInt     = JsonAST.JInt
  val  JInt     = JsonAST.JInt
  type JBool    = JsonAST.JBool
  val  JBool    = JsonAST.JBool
  type JField   = JsonAST.JField
  val  JField   = JsonAST.JField
  type JObject  = JsonAST.JObject
  val  JObject  = JsonAST.JObject
  type JArray   = JsonAST.JArray
  val  JArray   = JsonAST.JArray

  def parse(s: String): JValue = JsonParser.parse(s)
  def parseOpt(s: String): Option[JValue] = JsonParser.parseOpt(s)

  def render(value: JValue): Document = JsonAST.render(value)
  def compact(d: Document): String = Printer.compact(d)
  def pretty(d: Document): String = Printer.pretty(d)
}}
