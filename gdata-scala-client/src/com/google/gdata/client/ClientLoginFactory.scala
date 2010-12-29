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

import java.net.{URL, HttpURLConnection, URLEncoder}
import java.util.logging.Logger
import java.io.{BufferedWriter, OutputStreamWriter, IOException}

/**
 * Concrete factory class for ClientLogin authentication tokens.
 * 
 * @see AuthTokenFactory
 * @see http://code.google.com/apis/accounts/docs/AuthForInstalledApps.html
 * @author Iulian Dragos
 */
class ClientLoginFactory(appName: String, service: String) extends AuthTokenFactory {
  import ClientLoginFactory.{ClientLoginToken, logger}

  private var username: String = ""
  private var password: String = ""
  
  private var authToken: Option[AuthToken] = None
  
  /** 
   * Return the token held by this factory.
   * 
   * @throws IllegalStateException if the token was not set (either explicitly, or by
   *         calling <code>setUserCredentials</code>.
   */
  def token = {
    if (authToken.isDefined) 
      authToken.get
    else
      throw new IllegalStateException("Set user credentials before asking for a token.")
  }
  
  /** Set an authentication Token retrieved by other means. */
  def token_=(tok: AuthToken) {
    authToken = Some(tok)
    this
  }
  
  /** Obtain an authentication token for the given user/password. */
  def setUserCredentials(username: String, password: String) = {
    authToken = Some(getToken(username, password, None, None))
  }
    
  /** Obtain an authentication token for the given user/password with captcha challenge. */
  def setUserCredentials(username: String, 
                         password: String, 
                         captchaToken: String, 
                         captchaAnswer: String) = {
    authToken = Some(getToken(username, password, Some(captchaToken), Some(captchaAnswer)))
  }
  
  /** Make a ClientLogin request for the given user credentials.
   * 
   * @throws May throw a subclass of AuthenticationException, if authentication fails.
   * @throws IOException if an error occurs while opening the connection.
   * @throws SocketTimeoutException if timeout occurs before the connection is established.
   */
  private def getToken(username: String, 
                       password: String, 
                       captchaToken: Option[String], 
                       captchaAnswer: Option[String]): ClientLoginToken = {
    val url = new URL(ClientLoginFactory.ACCOUNT_URL)
    val connection = new HttpConnection(url.openConnection.asInstanceOf[HttpURLConnection])
    connection.doInput = true
    connection.doOutput = true
    connection.requestMethod = "POST"
    connection("Content-Type") = "application/x-www-form-urlencoded"
 
    val response = connection.connect

    val writer = new BufferedWriter(new OutputStreamWriter(connection.outputStream))
    var params = List(("Email", username),
                      ("Passwd", password), 
                      ("source", appName), 
                      ("service", service),
                      ("accountType", "HOSTED_OR_GOOGLE"))
    if (captchaToken.isDefined)
      params = ("logintoken", captchaToken.get) :: params
    if (captchaAnswer.isDefined)
      params = ("logincaptcha", captchaAnswer.get) :: params
      
    logger.fine("Sending ClientLogin request with " + params)
    writer.write(makeFormRequest(params))
    writer.close

    logger.fine("Got back headers: " + response.headers.mkString("\n", "\n", ""))
    val body = response.body
    logger.fine("Got back: " + body)
    if (response.responseCode != HttpURLConnection.HTTP_OK)
      throw AuthenticationException.fromServerReply(body)
      
    val t = parseToken(body)
    logger.fine("Got back token: " + t)
    new ClientLoginToken(t)
  }
  
  /** Return the token found in the response body. It looks for a line like 'Auth=<token>'. */
  private def parseToken(body: String): String = {
    val lines = body.split('\n')
    for (line <- lines) line.split('=') match {
      case Array("Auth", token) => return token
      case _ => ()
    }
    ""
  }
  
  /* Encode the given parameters using form encoding. */
  private def makeFormRequest(params: List[(String, String)]): String = {
    val sb = new StringBuilder
    for ((n, v) <- params)
      sb.append(URLEncoder.encode(n, "UTF-8"))
        .append('=')
        .append(URLEncoder.encode(v, "UTF-8"))
        .append('&')
    sb.deleteCharAt(sb.length - 1)
    sb.toString
  }
}

object ClientLoginFactory {
  final val ACCOUNT_URL = "https://www.google.com/accounts/ClientLogin"
  
  def logger = Logger.getLogger("com.google.gdata.client.ClientLoginFactory")
  
  /**
   * An authentication token for ClientLogin.
   */
  class ClientLoginToken(token: String) extends AuthToken {
    def getAuthHeader: String = {
      "GoogleLogin auth=" + token
    }
  }
}
