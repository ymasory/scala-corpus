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
import org.specs._
import org.specs.runner._

class fileSystemSpec extends SpecificationWithJUnit {
  shareVariables()
  "A FileSystem" should {
    "list all files in a directory with filePaths()" in {
      fs.filePaths("./src/test/scala/org/specs/io") mustContainMatch "fileSystemSpec"
    }
    "list all files in a directory assuming local directory if no point starts the path" in {
      fs.filePaths("src/test/scala/org/specs/io") mustContainMatch "fileSystemSpec"
    }
    "not list directories in a directory with filePaths()" in {
      fs.filePaths("./src/test/scala/org/specs") mustNotContainMatch "io$"
    }
    "list all files in a directory with filePaths() using a glob pattern" in {
      fs.filePaths("./src/test/scala/org/specs/io/*.*") mustContainMatch "fileSystemSpec"
      fs.filePaths("./src/test/scala/org/specs/**/*.*") mustContainMatch "fileSystemSpec"
    }
    "list file paths using a glob pattern like /dir/**/dir2/*.*" in {
      fs.filePaths("./**/io/*.*") mustContainMatch "fileSystemSpec"
    }
    "list file paths using /**/*name.ext and return only relevant files" in {
      fs.filePaths("./**/io/*mSpec.*") mustNotContainMatch "fileWriterSpec"
    }
    doAfter { fs.removeDir("./testingDir") }
    "remove a directory and all its content recursively with removeDir" in {
      fs.createDir("./testingDir/directoryToRemove")
      fs.createFile("./testingDir/directoryToRemove/testFile.txt")
      fs.removeDir("./testingDir")
      fs.exists("./testingDir") must beFalse
    }
    "remove a directory with removeDir and return the parent path" in {
      fs.createDir("./testingDir/directoryToRemove")
      fs.createFile("./testingDir/directoryToRemove/testFile.txt")
      fs.removeDir("./testingDir") must_== "."
    }
    "not remove a file with the removeDir method" in {
      fs.createDir("./testingDir/directoryToRemove")
      fs.createFile("./testingDir/directoryToRemove/testFile.txt")
      fs.removeDir("./testingDir/directoryToRemove/testFile.txt")
      fs.filePaths("./testingDir/directoryToRemove") must containMatch("testFile")
    }
  }
}
