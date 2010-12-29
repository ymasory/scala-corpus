/* Copyright (c) 2008 Google Inc.
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


package com.google.xml.combinators

/** 
 * A trait for extensible data. Unknown elements will be collected in 'store'.
 * 
 * This can be used for 'after the fact' extension of picklers. A user-defined 
 * type that mixes in HasStore must also use the 'extensible' combinator to 
 * collect remaining input. An extension can then be be applied to the collected
 * store: <code>extend(Person.pickler, extraElements)</code>
 */
trait HasStore {
  var store: XmlStore = LinearStore.empty
}
