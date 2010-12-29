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
package youtube

/**
 * A YouTube specific query.
 */
class YouTubeQuery extends Query {

  /** The parameter name for video queries. */
  override def searchName = "vq"
  
  /**
   * Specifies how to sort videos in the search result set. Valid values for this 
   * parameter are relevance, published, viewCount and rating. Default is 'relevance'. 
   */
  def orderBy(ordering: String): this.type =
    addParam("orderby", ordering)
  
  /**
   * Videos must be available in a particular format: use 1 for RTSP streaming, 5 for
   * videos that are embeddable, 6 for mobile video playback.
   */
  def format(format: Int): this.type = { 
    if (format != 1 && format != 5 && format != 6)
      throw new IllegalArgumentException("Unkown video format: " + format)
    addParam("format", String.valueOf(format))
  }

  /** Restrict to videos that have title, description or keywords in the given language. */
  def lr(lang: String): this.type =
    addParam("lr", lang)

  /** Include restricted content. Defaults to false. */
  def racy(b: Boolean): this.type =
    if (b) addParam("racy", "") else this
  
  /** 
   * The video will be played on 'target' (IP address or country code). Defaults to
   * the IP of the client. YouTube filters out videos that cannot be played in a given
   * country. Use this parameter when the IP of the client is different from the 
   * IP where the video will be played.
   */
  def restriction(target: String): this.type =
    addParam("restriction", target)
  
  /** 
   * Restrict search to videos uploaded in the giben time frame. Valid values are
   * 'today', 'this_week', 'this_month' and 'all_time'. This parameter is available 
   * only on 'top_rated', 'top_favorites', 'most_discussed', 'most_linked' and 
   * 'most_responded' standard feeds. 
   */
  def time(t: String): this.type = {
    addParam("time", t)
  }
}

object YouTubeQuery {
  def empty = new YouTubeQuery
}