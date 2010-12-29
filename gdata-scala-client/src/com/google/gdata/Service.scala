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

import com.google.gdata.client.{GDataRequest, RequestMethod, AuthTokenFactory, 
    ClientLoginFactory, MovedTemporarily}
import com.google.xml.combinators.Picklers.{Pickler, Success, NoSuccess}
import com.google.xml.combinators.{PlainOutputStore, LinearStore}
import com.google.gdata.data.kinds.{FeedLink, EntryLink}

import java.net.URL

import scala.xml.XML

/**
 * A base class for Google services. It provides the basic querying mechanism and
 * authentication. It caches 'gsessionid' parameters when redirected, and ships them 
 * with every future query.
 * 
 * All Google services should extend this class and provide specific methods for
 * that service. If a specific class is not yet available, this class can be used
 * with generic StdAtomFeed to retrieve and further process feeds.
 * 
 * @param appName The application name. It should contain the company name, application name
 *                and its version, separated by '-'.
 * @param service The service name. See <a href="http://code.google.com/support/bin/answer.py?answer=62712&topic=10433">
 * @author Iulian Dragos
 */
abstract class Service(appName: String, service: String) {
  private val userAgent = appName + " " + Service.SERVICE_NAME + "-" + Service.SERVICE_VERSION
  
  /** A factory for GData requests. */
  protected var requestFactory = new GDataRequest.Factory
  
  /** A facotry for authentication tokens. */
  protected var authTokenFactory: AuthTokenFactory = new ClientLoginFactory(appName, service)
  
  requestFactory += ("User-Agent", userAgent)

  protected var user: Option[String] = None
  
  /** Retrieve the current username. */
  def username: Option[String] = user
  
  /** Set user credentials. */
  def setUserCredentials(username: String, passwd: String) {
    user = Some(username)
    authTokenFactory.setUserCredentials(username, passwd)
    requestFactory.token = authTokenFactory.token
  }

  /** Set user credentials and captcha challenge. */
  def setUserCredentials(username: String, passwd: String, cToken: String, cAnswer: String) {
    user = Some(username)
    authTokenFactory.setUserCredentials(username, passwd, cToken, cAnswer)
    requestFactory.token = authTokenFactory.token
  }
  
  /**
   * Make the given query and parse the XML result using the given pickler.
   * 
   * @throws UnknownDocumentException if the pickler is unsuccessful.
   * @throws AuthenticationException (one of its subclasses) if the operation fails because
   *         of insufficient rights.
   * @throws IOException if there are connection issues.
   * @throws GDataRequestException if there are HTTP errors.
   */
  def query[A](base: String, q: Query, p: Pickler[A]): A = {
    query(q.mkUrl(base.toString), p)
  }
  
  /**
   * Make the given query and parse the XML result using the given pickler.
   * 
   * @throws UnknownDocumentException if the pickler is unsuccessful.
   * @throws AuthenticationException (one of its subclasses) if the operation fails because
   *         of insufficient rights.
   * @throws IOException if there are connection issues.
   * @throws GDataRequestException if there are HTTP errors.
   */
  def query[A](url: String, p: Pickler[A]): A =
    query(new URL(url), p)
  
  /**
   * Make the given query and parse the XML result using the given pickler.
   * 
   * @throws UnknownDocumentException if the pickler is unsuccessful.
   * @throws AuthenticationException (one of its subclasses) if the operation fails because
   *         of insufficient rights.
   * @throws IOException if there are connection issues.
   * @throws GDataRequestException if there are HTTP errors.
   */
  def query[A](url: URL, p: Pickler[A]): A = try {
    val request = requestFactory.mkRequest(RequestMethod.GET, url)
    request.unpickle(p) match {
      case Success(res, _) => 
        res
      case e: NoSuccess => 
        throw new UnknownDocumentException("The XML response could not be parsed.", e)
    }
  } catch {
    case m @ MovedTemporarily(headers) => query(handleRedirection(m), p)
  }
  
  /**
   * POST the given value to the given URL.
   * 
   * @throws UnknownDocumentException if the pickler is unsuccessful.
   * @throws AuthenticationException (one of its subclasses) if the operation fails because
   *         of insufficient rights.
   * @throws IOException if there are connection issues.
   * @throws GDataRequestException if there are HTTP errors.
   */
  final def insert[A](url: URL, v: A, pa: Pickler[A]): A = try {
    val request = requestFactory.mkRequest(RequestMethod.POST, url)
    pickle(request, v, pa)
  } catch {
    case m @ MovedTemporarily(headers) => insert(handleRedirection(m), v, pa)
  }
  
  /**
   * PUT the given value to the given URL. The given url should exactly match the
   * it of the entry that is updated.
   * 
   * @throws UnknownDocumentException if the pickler is unsuccessful.
   * @throws AuthenticationException (one of its subclasses) if the operation fails because
   *         of insufficient rights.
   * @throws IOException if there are connection issues.
   * @throws GDataRequestException if there are HTTP errors.
   */  
  final def update[A](url: URL, v: A, pa: Pickler[A]): A = try {
    val request = requestFactory.mkRequest(RequestMethod.PUT, url)
    pickle(request, v, pa)
  } catch {
    case m @ MovedTemporarily(headers) => update(handleRedirection(m), v, pa)
  }
  
  /**
   * DELETE the resource identified by the given URL.
   * 
   * @throws AuthenticationException (one of its subclasses) if the operation fails because
   *         of insufficient rights.
   * @throws IOException if there are connection issues.
   * @throws GDataRequestException if there are HTTP errors.
   */
  final def delete(url: URL) {
    val request = requestFactory.mkRequest(RequestMethod.DELETE, url)
    request.connect
  }
  
  /**
   * Pickle the given value on the request object and unpickle the result. The
   * request should allow both input and output, and should be in a state
   * that allows writing (no reading had yet been performed).
   * 
   * @throws UnknownDocumentException if the pickler is unsuccessful.
   * @throws AuthenticationException (one of its subclasses) if the operation fails because
   *         of insufficient rights.
   * @throws IOException if there are connection issues.
   * @throws GDataRequestException if there are HTTP errors.
   */
  protected def pickle[A](request: GDataRequest, v: A, pa: Pickler[A]): A = {
    val pickles = pa.pickle(v, PlainOutputStore.empty).rootNode
    request("Content-Type") = "application/atom+xml"
    val w = new java.io.OutputStreamWriter(request.outputStream)
    XML.write(w, pickles, "UTF-8", true, null)
    w.close
    request.connect
    try {
      pa.unpickle(LinearStore.fromInputStream(request.content)) match {
        case Success(res, _) =>
          res
        case e: NoSuccess =>
          throw new UnknownDocumentException("The XML response could not be parsed.", e)
      } 
    } finally {
      request.content.close
    }
  }

  /**
   * Return the redirection URL from the given exception. If the redirection URL
   * has a 'gsessionid' parameter, store it in the request factory for future
   * requests.
   */
  private def handleRedirection(m: MovedTemporarily): URL = {
    import java.util.regex.{Pattern, Matcher}
    
    m.headers.get("Location") match {
      case Some(redirectionUrl) =>
        val url = redirectionUrl.mkString("", "", "")
        Service.logger.fine("Redirecting to " + url)
        if (url.contains("gsessionid")) {
          val p = Pattern.compile(".*gsessionid=(\\w*).*")
          val m = p.matcher(url)
          if (m.matches)
            requestFactory.sessionId = m.group(1)
        }
        new URL(url)
      case None => 
        throw m
    }
  }

  /** 
   * Return the feed embedded in the given feed link, or make a query to retrieve
   * it from the given URL. If the url is not given, it assumes the feed is embedded.
   */
  def fromFeedLink[A](f: FeedLink[A], pa: Pickler[A]): A = {
    f.href match {
      case Some(href) => query(href, pa)
      case None => f.feed.get
    }
  }
  
  /** 
   * Return the entry embedded in the given entry link, or make a query to retrieve
   * it from the given URL. If the url is not given, it assumes the entry is embedded.
   */
  def fromEntryLink[A](f: EntryLink[A], pa: Pickler[A]): A = {
    f.href match {
      case Some(href) => query(href, pa)
      case None => f.entry.get
    }
  }
  
  protected def mkRequest(method: RequestMethod.Value, url: String) = {
    requestFactory.mkRequest(RequestMethod.GET, new URL(url))
  }
}

object Service {
  val logger = java.util.logging.Logger.getLogger("com.google.gdata.Service")

  final val SERVICE_NAME = "GData/Scala"
  final val SERVICE_VERSION = "0.1"
}
