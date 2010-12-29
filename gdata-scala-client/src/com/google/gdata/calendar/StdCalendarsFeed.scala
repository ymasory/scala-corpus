package com.google.gdata.calendar

import com.google.gdata.data.{AtomFeeds, kinds}

class StdCalendarsFeed extends AtomFeeds with CalendarEntries {
  type Feed = AtomFeed
  type Entry = CalendarEntry
  
  val contactEntries = new kinds.StdContactEntries
  
  def entryContentsPickler = calendarEntryContentsPickler
  def feedContentsPickler = atomFeedContentsPickler
}
