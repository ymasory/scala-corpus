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


package com.google.gdata

import com.google.xml.combinators.Picklers.NoSuccess

/**
 * Base class for GData Service exceptions.
 */
class ServiceException(msg: String) extends Exception(msg)

/**
 * This exception is thrown when the retrieved XML document cannot be parsed by the provided
 * pickler.
 */
case class UnknownDocumentException(msg: String, err: NoSuccess) extends ServiceException(msg) {
  override def toString = 
    msg + ": " + err
}
