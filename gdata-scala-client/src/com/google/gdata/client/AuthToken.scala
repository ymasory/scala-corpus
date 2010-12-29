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
 * An authentication token used by GData services. It encapsulate the logic necessary
 * to fill the 'Auth' HTTP header in GData requests. Such tokens are obtained using
 * an AuthFactory.
 * 
 * @see http://code.google.com/apis/gdata/auth.html for information on Google 
 *      authentication.
 */
trait AuthToken {
  /** 
   * Return the contents of the Auth: header field to be passed in GData requests that
   * are using this authentication token.
   */
  def getAuthHeader: String
}
