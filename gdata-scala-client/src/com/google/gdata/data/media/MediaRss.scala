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


package com.google.gdata.data.media

import com.google.xml.combinators.~
import com.google.xml.combinators.Picklers._
import com.google.gdata.data.Uris.mediaNs  
import com.google.util.Utility.printOptional

/**
 * Provide MediaRss elements media:group and media:content. It abstracts over the 
 * type of Group and Content, and provides base implementations for the two. Use this
 * trait by mixing it in and fixing the type of Group and Content to concrete types.
 * 
 * @see tests/ContentTest.scala
 * 
 * @author Iulian Dragos
 */
trait MediaRss {
  /** A type for media:group elements. */
  type Group <: BaseGroup
  
  def groupPickler: Pickler[Group] = elem("group", groupContentsPickler)(mediaNs)

  /** Abstract pickler for Group objects. */
  protected def groupContentsPickler: Pickler[Group]
  
  type Content <: BaseContent
  
  def contentPickler: Pickler[Content] = elem("content", contentContentsPickler)(mediaNs)
  
  /** Abstract pickler for Content objects. */
  protected def contentContentsPickler: Pickler[Content]

  /**
   * A media:content element. It describes the media object being syndicated.
   * 
   * @see http://search.yahoo.com/mrss
   * @author Iulian Dragos
   */
  class BaseContent {
    /** Direct url to media object. */
    var url: Option[String] = None
  
    /** File size in bytes. */
    var fileSize: Option[Int] = None
  
    /** MIME type of object. */
    var tpe: Option[String] = None
  
    /** Type of object: image | audio | video | document | executable. */
    var medium: Option[String] = None
  
    /** Is this the default object for the enclosing media group? */
    var isDefault: Option[Boolean] = None
  
    /** Is the object a sample of full version? sample | full | nonstop. */
    var expression: String = "full"
  
    /** Bitrate in kbs. */
    var bitrate: Option[Int] = None
  
    /** Number of frames per second */
    var framerate: Option[Double] = None
  
    /** Number of samples per second, in kHz. */
    var samplingrate: Option[Double] = None
  
    /** Number of audio channels. */
    var channels: Option[Int] = None
  
    /** Time length in seconds. */
    var duration: Option[Int] = None
  
    /** Height of the media object. */
    var height: Option[Int] = None
  
    /** Width of the media object. */
    var width: Option[Int] = None
  
    /** Primary language of the media object. */
    var lang: Option[String] = None
  
    /** Additional optional elements. */
    var group: Group = _
  
    /** Fill fields if this object. */
    def fillOwnFields(url: Option[String], fileSize: Option[Int], tpe: Option[String], medium: Option[String],
        isDefault: Option[Boolean], expression: String, bitrate: Option[Int], framerate: Option[Double],
        samplingrate: Option[Double], channels: Option[Int], duration: Option[Int], height: Option[Int],
        width: Option[Int], lang: Option[String], group: Group): this.type = {
      this.url = url
      this.fileSize = fileSize
      this.tpe = tpe
      this.medium = medium
      this.isDefault = isDefault
      this.expression = expression
      this.bitrate = bitrate
      this.framerate = framerate
      this.samplingrate = samplingrate
      this.channels = channels
      this.duration = duration
      this.height = height
      this.width = width
      this.lang = lang
      this.group = group
      this
    }
    
    def fromBaseContent(o: BaseContent) {
      this.url = o.url
      this.fileSize = o.fileSize
      this.tpe = o.tpe
      this.medium = o.medium
      this.isDefault = o.isDefault
      this.expression = o.expression
      this.bitrate = o.bitrate
      this.framerate = o.framerate
      this.samplingrate = o.samplingrate
      this.channels = o.channels
      this.duration = o.duration
      this.height = o.height
      this.width = o.width
      this.lang = o.lang
      this.group = o.group
    }
  
    override def toString = {
      val sb = new StringBuilder(256) // override default buffer size
      sb.append("Content: \n\t")
      printOptional(sb, " url", url)
      printOptional(sb, " fileSize", fileSize)
      printOptional(sb, " tpe", tpe)
      printOptional(sb, " medium", medium)
      printOptional(sb, " isDefault", isDefault)
      sb.append(" expression: ").append(expression)
      printOptional(sb, " bitrate", bitrate)
      printOptional(sb, " framerate", framerate)
      printOptional(sb, " samplingrate", samplingrate)
      printOptional(sb, " channels", channels)
      printOptional(sb, " duration", duration)
      printOptional(sb, " height", height)
      printOptional(sb, " width", width)
      printOptional(sb, " lang", lang)
      sb.append(group)
      sb.toString
    }
  }

  /** A pickler for media:contents subelements and attributes. */
  lazy val baseContentPickler: Pickler[BaseContent] =
    (wrap (opt(attr("url", text)) ~ opt(attr("fileSize", intVal)) ~ opt(attr("type", text))
        ~ opt(attr("medium", text)) ~ opt(attr("isDefault", boolVal))
        ~ default(attr("expression", text), "full") ~ opt(attr("bitrate", intVal))
        ~ opt(attr("framerate", doubleVal)) ~ opt(attr("samplingrate", doubleVal))
        ~ opt(attr("channels", intVal)) ~ opt(attr("duration", intVal))
        ~ opt(attr("height", intVal)) ~ opt(attr("width", intVal)) ~ opt(attr("lang", text))
        ~ groupContentsPickler) ({
      case url ~ fileSize ~ tpe ~ medium ~ isDefault ~ expression ~ bitrate ~ framerate 
          ~ samplingrate ~ channels ~ duration ~ height ~ width ~ lang ~ group => 
        (new BaseContent).fillOwnFields(url, fileSize, tpe, medium, isDefault, expression, bitrate, 
            framerate, samplingrate, channels, duration, height, width, lang, group)
     }) (fromBaseContent))

  private def fromBaseContent(c: BaseContent) = 
    (new ~(c.url, c.fileSize) ~ c.tpe ~ c.medium ~ c.isDefault ~ c.expression ~ c.bitrate
        ~ c.framerate ~ c.samplingrate ~ c.channels ~ c.duration ~ c.height ~ c.width
        ~ c.lang ~ c.group)

  
  /**
   * A media:group as defined by Media RSS. @see http://search.yahoo.com/mrss
   *
   * @author Iulian Dragos
   */
  class BaseGroup {
    /** Declares permissible audience. */
    var rating: Option[Rating] = None
  
    /** Title of the media object. */
    var title: Option[SimpleText] = None
  
    /** Short description, usually one sentence. */
    var description: Option[SimpleText] = None
  
    /** Relevant keywords describing the media object. */
    var keywords: List[String] = Nil
  
    /** Representative images for this media object. */
    var thumbnails: List[Thumbnail] = Nil
  
    /** Indicates the type of media content. */
    var category: Option[Category] = None
  
    /** Hash of the binary media object. */
    var hash: Option[Hash] = None
  
    /** Allows the media object to be accessed through a web browser player. */
    var player: Option[Player] = None
  
    /** Contributors to the creation of this media object. */
    var credits: List[Credit] = Nil
  
    /** Copyright information. */
    var copyright: Option[Copyright] = None
  
    /** Additional text transcripts, captions or lyrics. */
    var text: List[Text] = Nil
  
    /** Restrictions on the aggregator rendering this media object. Purely informational. */
    var restrictions: List[Restriction] = Nil
  
    /** Content descriptions. */
    var contentEntries: List[Content] = Nil
  
    /** Fill own fields. */
    def fillOwnFields(rating: Option[Rating], title: Option[SimpleText], description: Option[SimpleText], keywords: List[String],
        thumbnails: List[Thumbnail], category: Option[Category], hash: Option[Hash], player: Option[Player], credits: List[Credit],
        copyright: Option[Copyright], text: List[Text], restrictions: List[Restriction], contentEntries: List[Content]): this.type = {
      this.rating = rating
      this.title = title
      this.description = description
      this.keywords = keywords
      this.thumbnails = thumbnails
      this.category = category
      this.hash = hash
      this.player = player
      this.credits = credits
      this.copyright = copyright
      this.text = text
      this.restrictions = restrictions
      this.contentEntries = contentEntries
      this
    }
    
    def fromBaseGroup(g: BaseGroup) {
      fillOwnFields(g.rating, g.title, g.description, g.keywords, g.thumbnails, g.category, 
          g.hash, g.player, g.credits, g.copyright, g.text, g.restrictions, g.contentEntries)
    }
  
    override def toString: String = {
      val sb = new StringBuilder(256) // override default size of the internal buffer
      sb.append("Group: \n\t")
      printOptional(sb, " rating", rating)
      printOptional(sb, " title", title)
      printOptional(sb, " description", description)
      sb.append(" keywords: ").append(keywords.mkString("", ", ", ""))
      sb.append(" thumbnails: ").append(thumbnails)
      printOptional(sb, " category", category)
      printOptional(sb, " hash", hash)
      printOptional(sb, " player", player)
      sb.append(" credits: ").append(credits.mkString("", ", ", ""))
      printOptional(sb, " copyright", copyright)
      sb.append(" text: ").append(text.mkString("", ", ", ""))
      sb.append(" restrictions: ").append(restrictions.mkString("", ", ", ""))
      sb.append(" content: ").append(contentEntries.mkString("", ", ", ""))
      sb.toString
    }
  }
  
  private implicit val ns = mediaNs
  
  /** A pickler for the contents of a group. It has no enclosing 'media:group' element. */
  lazy val baseGroupPickler: Pickler[BaseGroup] = 
    (wrap (interleaved(opt(Rating.pickler) ~ opt(SimpleText.pickler("title"))
        ~ opt(SimpleText.pickler("description")) ~ default(Keywords.keywordsPickler, Nil) 
        ~ rep(Thumbnail.pickler) ~ opt(Category.pickler) ~ opt(Hash.pickler) ~ opt(Player.pickler)  
        ~ rep(Credit.pickler) ~ opt(Copyright.pickler) ~ rep(Text.pickler) ~ rep(Restriction.pickler)
        ~ rep(contentPickler))) ({
      case rating ~ title ~ description ~ keywords ~ thumbnails ~ category ~ hash ~ player 
          ~ credits ~ copyright ~ text ~ restrictions ~ content =>
        (new BaseGroup).fillOwnFields(rating, title, description, keywords, thumbnails, category, hash,
            player, credits, copyright, text, restrictions, content)
    }) (fromBaseGroup))
  
  private def fromBaseGroup(g: BaseGroup) = (new ~(g.rating, g.title) ~ g.description ~ g.keywords 
      ~ g.thumbnails ~ g.category ~ g.hash ~ g.player ~ g.credits ~ g.copyright ~ g.text
      ~ g.restrictions ~ g.contentEntries)
}
