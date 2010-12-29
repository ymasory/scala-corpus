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


package com.google.gdata.data;

/**
 * This object holds together common namespace definitions.
 */
object Uris {
  /** Atom namespace. Usually aliased to 'atom'. */
  final val ATOM = "http://www.w3.org/2005/Atom"

  /** An atom namespace binding. */
  lazy val atomNs = ("atom", ATOM)
  
  /** GData namespace. Usually aliased to 'gd'. */
  final val GDATA = "http://schemas.google.com/g/2005"

  /** A gdata namespace binding. */
  lazy val gdNs = ("gd", GDATA)
  
  /** XHTML namespace. Usually aliased to 'xhtml'. */
  final val XHTML = "http://www.w3.org/1999/xhtml"
  
  /** An xhtml namespace binding. */
  lazy val xhtmlNs = ("xhtml", XHTML)

  /** XML namespace. <b>Always</b> aliased to 'xml'. */
  final val XML = "http://www.w3.org/XML/1998/namespace"
  
  /** An xml namespace binding. */
  lazy val xmlNs = ("xml", XML)

  /** The Media RSS namespace. */
  final val MEDIA = "http://search.yahoo.com/mrss/"
  
  /** A media namespace binding. */
  lazy val mediaNs = ("media", MEDIA)
  
  /** Youtube API namespace. */
  final val YOUTUBE = "http://gdata.youtube.com/schemas/2007"
  
  /** A youtube namespace binding. */
  lazy val ytNs = ("yt", YOUTUBE)
  
  /** Open Search namespace. */
  final val OPENSEARCH = "http://a9.com/-/spec/opensearchrss/1.0/"
  
  /** An open search namespace binding. */
  lazy val openSearchNs = ("openSearch", OPENSEARCH)
  
  /** Google calendar namespace. */
  final val CALENDAR = "http://schemas.google.com/gCal/2005"
  
  /** A Google calendar namespace binding. */
  lazy val gCalNs = ("gCal", CALENDAR)
  
  /** The access control list namespace */
  final val ACL = "http://schemas.google.com/acl/2007"
  
  lazy val gAclNs = ("gAcl", ACL)
}
