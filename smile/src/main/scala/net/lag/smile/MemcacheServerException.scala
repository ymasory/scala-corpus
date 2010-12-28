/*
 * Copyright 2009 Twitter, Inc.
 * Copyright 2009 Robey Pointer <robeypointer@gmail.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License. You may obtain
 * a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.lag.smile

import java.io.IOException


/**
 * All exceptions thrown from this library will be subclasses of this exception.
 */
class MemcacheServerException(reason: String) extends IOException(reason)

class MemcacheServerTimeout extends MemcacheServerException("timeout")
class MemcacheServerOffline extends MemcacheServerException("server is unreachable")

class MemcacheClientError(reason: String) extends MemcacheServerException(reason)

class NotStoredException extends MemcacheClientError("not stored")
class KeyTooLongException extends MemcacheClientError("key too long")
