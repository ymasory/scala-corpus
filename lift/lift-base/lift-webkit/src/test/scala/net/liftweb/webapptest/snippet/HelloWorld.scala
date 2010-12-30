/*
 * Copyright 2007-2010 WorldWide Conferencing, LLC
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
package webapptest {
package snippet {

class HelloWorld {
  def howdy = <span>Welcome to webtest1 at {new _root_.java.util.Date}</span>
}

import scala.xml._
import net.liftweb.http._

class Meow extends Function1[NodeSeq, NodeSeq] {
  def apply(in: NodeSeq): NodeSeq = <yak/>
}

class Meower {
  def render: Meow = new Meow
}

class Splunker {
  def render = SHtml.onSubmit(s => ())
}


}
}
}
