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

private[scalatest] abstract class FeatureParser {

  type F
  def createFixture: F

  implicit def stringToZerosie(first: String) = new Zerosie(first)

  class Zerosie(val first: String) {
    def ___ (second: String): Onesie = new Onesie(first: String, second: String)
  }

  class Onesie(val first: String, val second: String) {
    def ___ (third: String): Twosie = new Twosie(first: String, second: String, third: String)
  }

  class Twosie (val first: String, val second: String, val third: String)

  def given(twosie: Twosie)(f: (F, String, String) => Unit) { /* register the function */ }

  val example = "my" ___ "account balance is $" ___ ""
}

