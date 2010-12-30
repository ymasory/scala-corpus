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
 * Trait formerly used to rerun tests or other entities (such as suites). <strong>Note: this trait
 * has been deprecated and will be removed in a future version of ScalaTest.</strong>
 *
 * <p>
 * <strong>As of version 1.0, trait <code>Rerunnable</code> is no longer used by the ScalaTest API. It has essentially been replaced
 * by trait <code>Rerunner</code>. It will be removed after a two-release deprecation cycle.
 * Please migrate any uses of <code>Rerunnable</code> to use trait <code>Rerunner</code> instead.</strong>
 * </p>
 *
 * @author Bill Venners
 */
trait Rerunnable {

  /**
   * <strong>Note: This trait has been deprecated and will be removed in a future version of ScalaTest.</strong>
   */
  @deprecated
  def rerun(reporter: Reporter, stopper: Stopper, includes: Set[String], excludes: Set[String],
      properties: Map[String, Any], distributor: Option[Distributor], loader: ClassLoader): Unit
}
