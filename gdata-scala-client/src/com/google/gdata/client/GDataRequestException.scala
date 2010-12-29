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

/** Base class for GData request exceptions. */
class GDataRequestException(msg: String) extends Exception(msg)

case class NotModifiedException() extends GDataRequestException(
    "Not modified.")

case class BadRequestException() extends GDataRequestException(
    "Invalid request URI or header, or unsupported nonstandard parameter.")

case class UnauthorizedException() extends GDataRequestException("Authorization required")

case class ForbiddenException() extends GDataRequestException(
    "Unsupported standard parameter, or authentication failed.")

case class NotFoundException() extends GDataRequestException(
    "Resource not found.")

case class ConflictException() extends GDataRequestException(
    "Specified version number doesn't match resource's latest version number.")

case class InternalServerErrorException() extends GDataRequestException(
    "Internal error.")

/** Redirection. The header collection should contain a 'Location' field. */
case class MovedTemporarily(headers: collection.Map[String, List[String]]) 
    extends GDataRequestException("Moved temporarily")
