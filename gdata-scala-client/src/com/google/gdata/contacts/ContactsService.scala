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
 * A service class for Google Contacts API. It provides conenience methods for
 * retrieving, adding, updating and deleting contacts.
 * 
 * The only feed described by this service is private, therefore authentication is 
 * required.
 * 
 * @author Iulian Dragos
 * @see http://code.google.com/apis/contacts/developers_guide_protocol.html
 */
class ContactsService(appName: String) extends Service(appName, "cp") {

  /** A contacts feed object. */
  val contactsFeed = new StdContactsFeed
  
  /**
   * Return the contacts list at the given URL.
   * 
   * @throws AuthenticationException (one of its subclasses) if the operation fails because
   *         of insufficient rights.
   * @throws IOException if there are connection issues.
   * @throws GDataRequestException if there are HTTP errors.
   * @throws UnknownDocumentException if the feed is not a contacts feed.
   */
  def getUserContacts(url: String): contactsFeed.Feed = {
    query(url, contactsFeed.feedPickler)
  }
  
  /**
   * Return the contacts list for the currently authenticated user.
   * 
   * @throws IllegalArgumentException if setUserCredentials was not set before.
   * @throws AuthenticationException (one of its subclasses) if the operation fails because
   *         of insufficient rights.
   * @throws IOException if there are connection issues.
   * @throws GDataRequestException if there are HTTP errors.
   */
  def getUserContacts(q: Query): contactsFeed.Feed =
    username match {
      case Some(name) => 
        getUserContacts(q.mkUrl(ContactsService.FEEDS + "/" + name + "/base"))
      case None => 
        throw new IllegalArgumentException(
            "Set user credentials before retrieving the default address book")
    }

  /**
   * Return the contacts list for the currently authenticated user.
   * 
   * @throws IllegalArgumentException if setUserCredentials was not set before.
   * @throws AuthenticationException (one of its subclasses) if the operation fails because
   *         of insufficient rights.
   * @throws IOException if there are connection issues.
   * @throws GDataRequestException if there are HTTP errors.
   */
  def getUserContacts: contactsFeed.Feed =
    getUserContacts(Query.empty)
}

object ContactsService {
  final val FEEDS = "http://www.google.com/m8/feeds/contacts"
}
