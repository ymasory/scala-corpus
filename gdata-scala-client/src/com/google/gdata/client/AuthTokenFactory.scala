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

/**
 * Factory for creating authentication objects. It encapsulates the method by which they
 * are aquired. It supports ClientLogin. TODO: AuthSub.
 * 
 * @see http://code.google.com/apis/gdata/auth.html
 * @author Iulian Dragos
 */
trait AuthTokenFactory {
  
  /** Return the current token. */
  def token: AuthToken
  
  /** Set the authentication token. */
  def token_=(t: AuthToken): Unit

  /** 
   * Sets the user credentials, and tries to get an authentication token. If authentication
   * fails, it throws an AuthenticationException
   * 
   * @throws may throw any of AuthenticationException subclasses.
   */
  def setUserCredentials(username: String, passwd: String): Unit
  
  /** 
   * Sets the user credentials, and tries to get an authentication token using the provided 
   * captcha token and answer. If authentication fails, it throws an AuthenticationException.  
   * 
   * @throws may throw any of AuthenticationException subclasses.
   */
  def setUserCredentials(username: String,
                         passwd: String,
                         captchaToken: String,
                         captchaAnswer: String): Unit
}
