/**
 * Copyright (c) 2007-2010 Eric Torreborre <etorreborre@yahoo.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
 * documentation files (the "Software"), to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of
 * the Software. Neither the name of specs nor the names of its contributors may be used to endorse or promote
 * products derived from this software without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
package org.specs.specification
import org.specs.matcher._
import org.specs.runner._
import org.specs._

class taggedSpec extends SpecificationWithJUnit {
  "A tagged object" should { createTagged.before
    "be accepted if there is no tag added" in {
      tagged must beAccepted
    }
    "be accepted if there is no tag added and only some tags are accepted" in {
      tagged.accept("a") must beRejected
    }
    "be included if it accepts some tags it owns" in {
      tagged.addTag("a").accept("a") must beAccepted
    }
    "be accepted if a tag is not rejected" in {
      tagged.addTag("a") must beAccepted
    }
    "be rejected if a tag is rejected" in {
      tagged.addTag("a").reject("a") must beRejected
    }
    "be rejected if a tag is not in the accepted tags" in {
      tagged.addTag("a").accept("b") must beRejected
    }
    "be rejected if a tag is in the accepted tags but also in the rejected tags" in {
      tagged.addTag("a").accept("a").reject("a") must beRejected
    }
    "be accepted if a tag is in the accepted tags but not in the rejected tags" in {
      tagged.addTag("a").accept("a").reject("b") must beAccepted
    }
    "be taggable with another tagged object" in {
      val other = (new Object with Tagged).addTag("a").accept("a").reject("b")

      tagged.tagWith(other)
      tagged.tagList must_== other.tagList
      tagged.accepted must_== other.accepted
      tagged.rejected must_== other.rejected
    }
  }
  "A Tag object" should {
    "match another tag with the same name" in {
      Tag("hello") must beMatching(Tag("hello"))
    }
    "match another tag with a regex" in {
      Tag("hello") must beMatching(Tag("h.*"))
    }
    "match another tag with a name if it is itself a regex" in {
      Tag("h.*") must beMatching(Tag("hello"))
    }
    "match another tag with a name if both are regexes" in {
      Tag("h.*") must beMatching(Tag("h.*"))
    }
    "not match another tag even if its regex is not formed ok" in {
      Tag("h.*") must beMatching(Tag("h][")).not
    }
    "not match another tag even if it is null" in {
      Tag("h.*") must beMatching(Tag(null)).not
    }
    "not match another tag even if it is itself null" in {
      Tag(null) must beMatching(Tag("sdf")).not
    }
  }
  "A tagged object with subcomponents" should { createTaggedTree.before
    "propagate its tags to the subcomponents" in {
      taggedTree.tag("1")
      taggedTree.taggedComponents.head.tagNames must haveSameElementsAs(List("1"))
    }
    "clear the subcomponents tags when clearing its own" in {
      taggedTree.tag("1")
      taggedTree.clearTags
      taggedTree.taggedComponents.head.tagList must beEmpty
    }
    "be able to accept all tags if some tags were previously rejected" in {
      taggedTree.tag("1")
      taggedTree.acceptTag("1")
      taggedTree.acceptAnyTag
      taggedTree.taggedComponents.head.accepted must beEmpty
      taggedTree.taggedComponents.head.rejected must beEmpty
    }
  }
  var tagged = new Object with Tagged
  var taggedTree = new Object with Tagged
  def createTagged = tagged = new Object with Tagged
  def createTaggedTree = {
    val child = new Object with Tagged
    taggedTree = new Object with Tagged { override def taggedComponents = List(child) }
  }
  def beRejected = beAccepted.not
  def beAccepted = new Matcher[Tagged] {
    def apply(v: => Tagged) = (v.isAccepted, "the tag is accepted", "the tag isn't accepted")
  }
  def beMatching(other: Tag) = new Matcher[Tag] {
    def apply(v: => Tag) = (v.matches(other), v + " matches " + other, v + " doesn't match " + other)
  }
}
