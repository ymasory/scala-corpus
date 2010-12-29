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


package com.google.gdata.client

import java.net.URL

/**
 * A factory for GData requests. It handles header parameters that need to be set
 * on all requests, as well as authentication tokens.
 *
 * @author Iulian Dragos
 */
trait RequestFactory {
  /** 
   * Create a request given a method and a URL. The request will contain the authentication
   * token, if any, and all header parameters set on this factory.
   */
  def mkRequest(method: RequestMethod.Value, url: URL): GDataRequest
  
  /** Set the authentication token. All requests will get this token. */
  def token_=(token: AuthToken)
  
  /** Get the current authentication token, if any. */
  def token: Option[AuthToken]
  
  /** Add a new header parameter that will be set on all requests created by this factory. */
  def +=(name: String, value: String): this.type
}
