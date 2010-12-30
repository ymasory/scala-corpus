/*
 * Copyright 2001-2008 Artima, Inc.
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
package org.scalatest.matchers

import org.scalatest._
import scala.collection.mutable.ListBuffer

trait CustomMatchers {

  class FileExistsMatcher extends Matcher[java.io.File] {

    def apply(left: java.io.File) = {

      val fileOrDir = if (left.isFile) "file" else "directory"

      val failureMessageSuffix = 
        fileOrDir + " named " + left.getName + " did not exist"

      val negatedFailureMessageSuffix = 
        fileOrDir + " named " + left.getName + " existed"

      MatchResult(
        left.exists,
        "The " + failureMessageSuffix,
        "The " + negatedFailureMessageSuffix,
        "the " + failureMessageSuffix,
        "the " + negatedFailureMessageSuffix
      )
    }
  }

  val exist = new FileExistsMatcher
}

class CustomMatcherSpec extends Spec with ShouldMatchers with CustomMatchers {

  describe("A customer matcher") {

    it("should work when used in various combinations of and, or, and not, when the file does not exist") {

      val imaginaryFile = new java.io.File("imaginary.txt")

      imaginaryFile should not (exist)

      val caught1 = intercept[TestFailedException] {
        imaginaryFile should exist
      }
      assert(caught1.getMessage === "The directory named imaginary.txt did not exist")

      imaginaryFile should (not be a ('file) and not (exist))

      val caught2 = intercept[TestFailedException] {
        imaginaryFile should (not be a ('file) and exist)
      }
      assert(caught2.getMessage === "imaginary.txt was not a file, but the directory named imaginary.txt did not exist")
    }

    it("should work when used in various combinations of and, or, and not, when the file does exist") {

      val tempFile = java.io.File.createTempFile("delete", "me")

      try {
        tempFile should exist

        val caught1 = intercept[TestFailedException] {
          tempFile should not (exist)
        }
        assert(caught1.getMessage === "The file named " + tempFile.getName + " existed")
        caught1.getMessage should startWith ("The file named delete")
        caught1.getMessage should endWith ("me existed")

        tempFile should (be a ('file) and exist)

        val caught2 = intercept[TestFailedException] {
          tempFile should (be a ('file) and not (exist))
        }
        caught2.getMessage should endWith (", but the file named " + tempFile.getName + " existed")
      }
      finally {
        tempFile.delete()
      }
    }
  }
}
 
