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
package org.specs.io
import org.specs.runner._

class fileSystemUnit extends TestData {
  "A file system" should {
    "provide a globToPattern function returning the regex pattern corresponding to a glob definition" in {
      paths must pass { matchingPath: MatchingPath =>
        matchingPath.path must beMatching(globToPattern(matchingPath.glob))
      } set (minTestsOk->5)
    }
  }
}
import org.scalacheck.Gen._
import org.scalacheck._
import scala.collection.mutable.Queue
import java.util.regex._
import org.specs._
import org.specs.Sugar._

class TestData extends SpecificationWithJUnit with FileSystem with ConsoleOutput with ScalaCheck {
  case class MatchingPath(path: String, glob: String)
  def paths = for { glob <- Gen.oneOf("src/**/*.*", "src/**/hello/**/*.*", "src/test/*.*")
                    path <- Gen.oneOf(pathsMatchingGlob(glob))
                  } yield MatchingPath(path, glob)

  def pathsMatchingGlob(glob: String): List[String] = {
    for { doubleStar   <- List("dir", "dir1/dir2")
          specialChar <-  "!@#$%^&';{}[]".iterator.toList
          name         <- List("name", "name" + specialChar, "name2")
          ext          <- List("ext1", "ext2")
        } yield "./" + glob.replace("**", doubleStar).replace(".*", "." + ext).replace("*", name)
  }
}
