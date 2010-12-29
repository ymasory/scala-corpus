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

import com.google.util.Utility.option

import java.net.{HttpURLConnection, URL}
import java.io.{InputStream, OutputStream, InputStreamReader, BufferedReader}
import java.nio.charset.Charset
import java.security.Permission

import scala.collection.{mutable, immutable}

/**
 * Scala wrapper over HttpURLConnection. Turns getter/setters into properties and implements
 * a request headers as properties. Like the underlying Java connection, it
 * supports only one request/response. The 'connect' method returns an <code>HttpResponse</code>
 * that gives access to the response headers and streams.
 *
 * You can set headers using the following syntax:
 * <code>
 *   connection("Content-Type") = "text/xml"
 * </code>
 *
 * A similar syntax can be used on the HttpResponse object to retrieve response headers:
 * <code>
 *   val response = connection.connect
 *   log("redirected to: " + response("Location"))
 * </code>
 *
 * @author Iulian Dragos
 */
class HttpConnection(val underlying: HttpURLConnection) {
  /** Get the error stream of this connection. */
  def errorStream: InputStream = underlying.getErrorStream

  /** Get an output stream that writes to this connection. */
  def outputStream: OutputStream = underlying.getOutputStream

  /** Get the n'th header field. */
  def apply(n: Int): Option[String] = option(underlying.getHeaderField(n))

  /** Get the given header field, parsed as date. */
  def headerFieldDate(name: String, default: Long): Long =
    underlying.getHeaderFieldDate(name, default)

  /** Get the key of the n'th field */
  def headerFieldKey(n: Int) = underlying.getHeaderFieldKey(n)

  /** Is this instance following redirects? */
  def instanceFollowRedirects: Boolean = underlying.getInstanceFollowRedirects

  /** Sets whether this instance should follow redirects. */
  def instanceFollowRedirects_=(b: Boolean) = underlying.setInstanceFollowRedirects(b)

  /** The permission object representing the permission necessary to make the connection. */
  def permission: Permission = underlying.getPermission

  /** Get the request method */
  def requestMethod: String = underlying.getRequestMethod

  /** Set the request method. */
  def requestMethod_=(m: String) = underlying.setRequestMethod(m)

  /** Get the connection timeout. */
  def connectTimeout: Int = underlying.getConnectTimeout

  /** Set the connection timeout. */
  def connectTimeout_=(n: Int) = underlying.setConnectTimeout(n)

  /** Controls whether this connection is used for input. */
  def doInput: Boolean = underlying.getDoInput

  /** Controls whether this connection is used for input. */
  def doInput_=(b: Boolean) = underlying.setDoInput(true)

  /** Controls whether this connection is used for output. */
  def doOutput: Boolean = underlying.getDoOutput

  /** Controls whether this connection is used for output. */
  def doOutput_=(b: Boolean) = underlying.setDoOutput(b)

  /** Controls whether caches can be used for this connection. */
  def useCaches: Boolean = underlying.getUseCaches

  /** Controls whether caches can be used for this connection. */
  def useCaches_=(b: Boolean) = underlying.setUseCaches(b)

  /** The URL of this connection. */
  def url: URL = underlying.getURL

  override def toString = underlying.toString

  def update(name: String, value: String) =
    underlying.addRequestProperty(name, value)

  /** Connect and return an response object. */
  def connect: HttpResponse = {
    underlying.connect
    new HttpResponse
  }

  /**
   * An http response is a map of header names to values. An instance of this class
   * is returned from the 'connect' method.
   */
  class HttpResponse extends (String => Option[String]) {
    /** The HTTP response code. */
	def responseCode: Int = underlying.getResponseCode

	/** The HTTP response message (reason) returned along with the response code. */
	def responseMessage: String = {
	  val m = underlying.getResponseMessage
	  if (m eq null) "" else m
	}

    /** Content encoding header. */
    def contentEncoding: Option[String] = option(underlying.getContentEncoding)

    /** Content length, or -1 if not known. */
    def contentLength: Int = underlying.getContentLength

    /** Content type value. */
    def contentType: Option[String] = option(underlying.getContentType)

    /** The date field. */
    def date: Long = underlying.getDate

    /** The value of the 'expires' header field. */
    def expiration: Long = underlying.getExpiration

    /** The value of the 'ifModifiedSince' header field. */
    def ifModifiedSince: Long = underlying.getIfModifiedSince

    /** Get an input stream that reads from this connection. */
    def inputStream: InputStream = underlying.getInputStream

    /** Get the value of 'last-modified' header field. */
    def lastModified: Long = underlying.getLastModified

    /**
     * Return the body of this response, using the content encoding header, if any. If
     * there is none, it assumes 'UTF-8' encoding.
     *
     * @throws IllegalCharsetNameException if the encoding name is illegal.
     * @throws UnsupportedCharsetException if the encoding is not available.
     */
    def body: String = {
      contentEncoding match {
        case Some(enc) => body(enc)
        case None => body("UTF-8")
      }
    }

    /**
     * Return the body of this response, as String, using the given encoding.
     *
     * @throws IllegalCharsetNameException if the encoding name is illegal.
     * @throws UnsupportedCharsetException if the encoding is not available.
     */
    def body(encoding: String): String = {
      val inStream =
        if (responseCode == HttpURLConnection.HTTP_OK)
          inputStream
        else
          errorStream

      try {
        val output = new StringBuilder
        val reader = new BufferedReader(
            new InputStreamReader(inStream, Charset.forName(encoding)))
        var line = reader.readLine

        while (line ne null) {
          output.append(line).append('\n')
          line = reader.readLine
        }
        output.toString
      } finally {
        inStream.close
      }
    }

    /**
     * Return a map of all headers in the server's respones. It does not return the
     * HTTP response code and message (which are mapped to the 'null' key by the Java
     * underlying method). They are accessible through the <code>responseCode</code> and
     * <code>responseMessage</code> methods.
     */
    lazy val headers: collection.Map[String, List[String]] = {
      def toScalaList[A](l: java.util.List[A]): List[A] = {
        val ab = new mutable.ListBuffer[A]
        val iter = l.iterator
        while (iter.hasNext) ab += iter.next
        ab.toList
      }

      import collection.JavaConversions.asMap
      val res: mutable.Map[String, List[String]] = new mutable.HashMap

      for ((key, headers) <- asMap(underlying.getHeaderFields) if key ne null)
        res(key) = toScalaList(headers)

      res
    }

    /** Return the header field name 'name', if it exists. */
    def apply(name: String): Option[String] =
      option(underlying.getHeaderField(name))

    override def toString =
      "HttpResponse of " + HttpConnection.toString
  }
}

object HttpConnection {
  /** HTTP redirects (3xx) should be automatically followed? */
  def followRedirects: Boolean = HttpURLConnection.getFollowRedirects

  /** Sets whether HTTP redirects should be automatically followed. */
  def followRedirects_=(b: Boolean) = HttpURLConnection.setFollowRedirects(b)
}
