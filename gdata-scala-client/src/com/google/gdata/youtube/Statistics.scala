package com.google.gdata.youtube

import com.google.xml.combinators.{Picklers, ~}
import com.google.gdata.data.util.DateTime
import com.google.gdata.data.Uris

/**
 * You tube statistics. Used in user profile entries.
 * 
 * @author Iulian Dragos
 */
case class Statistics(viewCount: Option[Int], videoWatchCount: Option[Int], 
    subscriberCount: Option[Int], lastWebAccess: Option[DateTime])

object Statistics {
  import Picklers._
  
  /** A pickler for yt:statistics. */
  def pickler: Pickler[Statistics] =
    (wrap (elem("statistics", opt(attr("viewCount", intVal)) ~ opt(attr("videoWatchCount", intVal))
        ~ opt(attr("subscriberCount", intVal)) ~ opt(attr("lastWebAccess", dateTime)))(Uris.ytNs))
        (Statistics.apply) (fromStatistics))
  
  private def fromStatistics(s: Statistics) = 
    new ~(s.viewCount, s.videoWatchCount) ~ s.subscriberCount ~ s.lastWebAccess
}