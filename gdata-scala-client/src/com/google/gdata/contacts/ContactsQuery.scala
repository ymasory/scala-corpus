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
package contacts

/**
 * A Contacts API query. It adds service specific query parameters, like ordering,
 * sorting and whether deleted contacts should be included.
 * 
 * @author Iulian Dragos
 * @see http://code.google.com/apis/contacts/reference.html
 */
class ContactsQuery extends Query {

  /** Specifiy an ordering. Currently only 'lastmodified' is supported. */
  def orderBy(ordering: String): this.type =
    addParam("orderby", ordering)
  
  /** Specify whether deleted contacts should be included (defaults to false). */
  def showdeleted(b: Boolean): this.type =
    addParam("showdeleted", String.valueOf(b))
  
  /** Sorting order, can be either 'ascending' or 'descendig'. */
  def sortorder(s: String): this.type =
    addParam("sortorder", s)
}
