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

import java.util.logging.Logger
  
/**
 * Base class for Google authentication exceptions.
 */
abstract class AuthenticationException(val error: String) extends Exception(error) {
  def url: String
}

/**
 * Factory for authentication exceptions, based on server answer. 
 */
object AuthenticationException {
  val logger = Logger.getLogger("com.google.gdata.AuthenticationException")
  
  /** Create the right exception based on the reply from the server. */
  def fromServerReply(body: String): AuthenticationException = {
    val lines = body.split('\n')
    var errorCode, url, captchaToken, captchaUrl = ""
    
    for (l <- lines) l.split('=')(0) match {
      case "Error" => errorCode = l.substring(l.indexOf("=") + 1)
      case "Url" => url = l.substring(l.indexOf("=") + 1)
      case "CaptchaToken" => l.substring(l.indexOf("=") + 1)
      case "CaptchaUrl" => l.substring(l.indexOf("=") + 1)
      case _ => logger.info("Unknown field in response: " + l)
    }
    errorCode match {
      case "BadAuthentication" => BadAuthentication(url)
      case "NotVerified" => NotVerified(url)
      case "TermsNotAgreed" => TermsNotAgreed(url)
      case "CaptchaRequired" => CaptchaRequired(url, captchaToken, captchaUrl)
      case "AccountDeleted" => AccountDeleted(url)
      case "AccountDisabled" => AccountDisabled(url)
      case "ServiceDisabled" => ServiceDisabled(url)
      case "ServiceUnavailable" => ServiceUnavailable(url)
      case _ => Unknown(errorCode) // it matches unknown codes, including 'Unknown'
    }
  }
}

/** Wrong username or password. */
case class BadAuthentication(url: String) 
    extends AuthenticationException("Username or password not recognized.")

/** Account not verified. */
case class NotVerified(url: String)
    extends AuthenticationException("The account email address has not been verified.")

/** Terms not agreed. */
case class TermsNotAgreed(url: String)
    extends AuthenticationException("The user has not agreed to terms.")

/** 
 * The user should answer a captcha challenge. The captcha url points to the image. Reattempt
 * authentication by providing user's answer and the captcha token.
 */
case class CaptchaRequired(url: String, token: String, captchaUrl: String)
    extends AuthenticationException("A CAPTCHA is required.")

/** Unknown error. */
case class Unknown(url: String)
    extends AuthenticationException("Unknown error. Malformed request or invalid input. " + url)

/** The account has been deleted. */
case class AccountDeleted(url: String)
    extends AuthenticationException("The user account has been deleted.")

/** The account has been disabled. */
case class AccountDisabled(url: String)
    extends AuthenticationException("The user account has been disabled.")
    
/** Service is disabled for the given user. */
case class ServiceDisabled(url: String)
    extends AuthenticationException("The user's access to the specified service has been disabled.")

/** The service is unavailable. */
case class ServiceUnavailable(url: String)
    extends AuthenticationException("The service is not available; try again later.")
