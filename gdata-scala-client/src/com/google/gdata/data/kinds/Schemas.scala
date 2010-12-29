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


package com.google.gdata.data.kinds

/**
 * Defines URIs used in GData kinds to fill in 'rel' attributes for 
 * phone and email address, etc.
 * 
 * @author Iulian Dragos
 * @see http://code.google.com/apis/gdata/elements.html
 */
object Schemas {
  final val HOME = "http://schemas.google.com/g/2005#home"
  final val WORK = "http://schemas.google.com/g/2005#work"
  final val OTHER = "http://schemas.google.com/g/2005#other"
  final val CAR = "http://schemas.google.com/g/2005#car"
  final val MOBILE = "http://schemas.google.com/g/2005#mobile"
  final val PAGER = "http://schemas.google.com/g/2005#pager"
  final val SATELLITE = "http://schemas.google.com/g/2005#satellite"
  final val VOIP = "http://schemas.google.com/g/2005#voip"
  final val FAX = "http://schemas.google.com/g/2005#fax"
  final val HOME_FAX = "http://schemas.google.com/g/2005#home_fax"
  final val WORK_FAX = "http://schemas.google.com/g/2005#work_fax"

  final val GENERAL = "http://schemas.google.com/g/2005#general"
  final val INTERNAL_EXTENTION = "http://schemas.google.com/g/2005#internal-extension"
  
  final val EVENT = "http://schemas.google.com/g/2005#event"
  final val EVENT_ALT = "http://schemas.google.com/g/2005#event.alternate"
  final val EVENT_PARKING = "http://schemas.google.com/g/2005#event.parking"
  
  final val KIND = "http://schemas.google.com/g/2005#kind"
}
