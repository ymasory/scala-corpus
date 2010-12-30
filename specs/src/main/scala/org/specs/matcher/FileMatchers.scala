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
package org.specs.matcher
import java.io.File
import org.specs.matcher.MatcherUtils.{q, matches}
import org.specs.io.{FileSystem, ConsoleOutput}
import org.specs.specification.Result
/**
 * The <code>PathMatchers</code> trait provides matchers which are applicable to strings representing paths
 */
trait PathMatchers extends PathBaseMatchers with PathBeHaveMatchers
trait PathBaseMatchers extends FileSystem { outer =>
  /**
   * Matches if new File(path).exists
   */   
  def beAnExistingPath = new Matcher[String](){ 
    def apply(v: =>String) = {val path = v; (path != null && exists(path), d(path) + " exists", d(path) + " doesn't exist")} 
  } 
  /** alias for beAnExistingPath */
  def existPath = beAnExistingPath 

  /**
   * Matches if new File(path).canRead
   */   
  def beAReadablePath = new Matcher[String](){ 
    def apply(v: =>String) = {val path = v; (path != null && canRead(path), d(path) + " is readable", d(path) + " can't be read")} 
  }
  /**
   * Matches if new File(path).canWrite
   */   
  def beAWritablePath = new Matcher[String](){ 
    def apply(v: =>String) = {val path = v; (path != null && canWrite(path), d(path) + " is writable", d(path) + " can't be written")} 
  } 
  /**
   * Matches if new File(path).isAbsolute
   */   
  def beAnAbsolutePath = new Matcher[String](){ 
    def apply(v: =>String) = {val path = v; (path != null && isAbsolute(path), d(path) + " is absolute", d(path) + " is not absolute")} 
  } 
  /**
   * Matches if new File(path).isHidden
   */   
  def beAHiddenPath = new Matcher[String](){ 
    def apply(v: =>String) = {val path = v; (path != null && isHidden(path), d(path) + " is hidden", d(path) + " is not hidden")} 
  } 
  /**
   * Matches if new File(path).isFile
   */   
  def beAFilePath = new Matcher[String](){ 
    def apply(v: =>String) = {val path = v; (path != null && isFile(path), d(path) + " is a file", d(path) + " is not a file")} 
  } 
  /**
   * Matches if new File(path).isDirectory
   */   
  def beADirectoryPath = new Matcher[String](){ 
    def apply(v: =>String) = {val path = v; (path != null && isDirectory(path), d(path) + " is a directory", d(path) + " is not a directory")} 
  } 
  /**
   * Matches if new File(path).getName == name
   */   
  def havePathName(name: String) = new Matcher[String](){ 
    def apply(v: =>String) = {val path = v; (isEqualIgnoringSep(getName(path), name), d(path) + " is named " + q(name), d(path) + " is not named " + q(name))} 
  } 
  /**
   * Matches if new File(path).getAbsolutePath == absolutePath
   */   
  def haveAsAbsolutePath(absolutePath: String) = new Matcher[String](){ 
    def apply(v: =>String) = {val path = v; (isEqualIgnoringSep(path, absolutePath), d(path) + " has absolute path " + q(absolutePath), d(path) + " doesn't have absolute path " + q(absolutePath) + " but " + q(getAbsolutePath(path)))} 
  } 
  /**
   * Matches if new File(path).getCanonicalPath == canonicalPath
   */   
  def haveAsCanonicalPath(canonicalPath: String) = new Matcher[String](){ 
    def apply(v: =>String) = {val path = v; (isEqualIgnoringSep(getCanonicalPath(path), canonicalPath), d(path) + " has canonical path " + q(canonicalPath), d(path) + " doesn't have canonical path " + q(canonicalPath) + " but " + q(getCanonicalPath(path)))} 
  } 
  /**
   * Matches if new File(path).getParent == parent
   */   
  def haveParentPath(parent: String) = new Matcher[String](){ 
    def apply(v: =>String) = {val path = v; (isEqualIgnoringSep(getParent(path), parent), d(path) + " has parent path " + q(parent), d(path) + " doesn't have parent path " + q(parent) + " but " + q(getParent(path)))} 
  } 
  /**
   * Matches if new File(path).list == list(files)
   */   
  def listPaths(list: String*) = new Matcher[String](){ 
    def apply(v: =>String) = {val path = v; (path != null && list != null && listFiles(path).toList == list.toList, 
                                         d(path) + " has files " + q(list.mkString(", ")), 
                                         d(path) + " doesn't have files " + q(list.toList.mkString(", ")) + " but " + q(listFiles(path).toList.mkString(", ")))} 
  } 
  /**
   * Matches if 2 paths are the same regardless of their separators
   * @deprecated use beEqualToIgnoringSep instead
   */   
  def beEqualIgnoringSep(other: String) = beEqualToIgnoringSep(other)
  /**
   * Matches if 2 paths are the same regardless of their separators
   */   
  def beEqualToIgnoringSep(other: String) = new Matcher[String](){ 
    def apply(v: =>String) = {val path = v; (isEqualIgnoringSep(path, other) , d(path) + " is equal ignoring separators to " + q(other), d(path) + " is not equal ignoring separators to " + q(other))} 
  }
  /** @return true if the 2 paths are equal, ignoring separators */
  def isEqualIgnoringSep(path: String, other: String) = path != null && other != null&& getCanonicalPath(path).replaceAll("\\\\", "/") == getCanonicalPath(other).replaceAll("\\\\", "/") 
}
trait PathBeHaveMatchers { outer: PathBaseMatchers =>
  /** 
   * matcher aliases and implicits to use with BeVerb and HaveVerb 
   */
  implicit def toPathResultMatcher(result: Result[String]) = new PathResultMatcher(result)
  class PathResultMatcher(result: Result[String]) {
    def existingPath = result.matchWith(outer.existingPath)
    def hiddenPath = result.matchWith(outer.hiddenPath)
    def readablePath = result.matchWith(outer.readablePath)
    def writablePath = result.matchWith(outer.writablePath)
    def absolutePath = result.matchWith(outer.absolutePath)
    def filePath = result.matchWith(outer.filePath)
    def directoryPath = result.matchWith(outer.directoryPath)
    def pathName(name: String) = result.matchWith(havePathName(name))
    def listPaths(list: String*) = result.matchWith(outer.listPaths(list:_*))
    def asAbsolutePath(path: String) = result.matchWith(haveAsAbsolutePath(path))
    def asCanonicalPath(path: String) = result.matchWith(haveAsCanonicalPath(path))
    def parentPath(path: String) = result.matchWith(haveParentPath(path))
    def equalIgnoringSepTo(other: String) = result.matchWith(beEqualToIgnoringSep(other))
    def equalToIgnoringSep(other: String) = result.matchWith(beEqualToIgnoringSep(other))
  }
  def existingPath = beAnExistingPath 
  def hiddenPath = beAHiddenPath 
  def readablePath = beAReadablePath
  def writablePath = beAWritablePath
  def absolutePath = beAnAbsolutePath
  def filePath = beAFilePath
  def directoryPath = beADirectoryPath
  def pathName(name: String) = havePathName(name)
  def asAbsolutePath(path: String) = haveAsAbsolutePath(path)
  def asCanonicalPath(path: String) = haveAsCanonicalPath(path)
  def parentPath(path: String) = haveParentPath(path)
  def equalIgnoringSepTo(other: String) = beEqualToIgnoringSep(other)
  def equalToIgnoringSep(other: String) = beEqualToIgnoringSep(other)
}
/**
 * The <code>FileMatchers</code> trait provides matchers which are applicable to files
 */
trait FileMatchers extends FileBaseMatchers with FileBeHaveMatchers
trait FileBaseMatchers extends PathMatchers {
  /**
   * Matches if file.exists
   */   
  def exist[T <: { def getPath(): String }] = (existPath) ^^ ((_:T).getPath)

  /**
   * Matches if file.canRead
   */   
  def beReadable[T <: { def getPath(): String }] = (beAReadablePath) ^^ ((_:T).getPath)

  /**
   * Matches if file.canWrite
   */   
  def beWritable[T <: { def getPath(): String }] = (beAWritablePath) ^^ ((_:T).getPath)

  /**
   * Matches if file.isAbsolute
   */   
  def beAbsolute[T <: { def getPath(): String }] = (beAnAbsolutePath) ^^ ((_:T).getPath)

  /**
   * Matches if file.isHidden
   */   
  def beHidden[T <: { def getPath(): String }] = (beAHiddenPath) ^^ ((_:T).getPath)

  /**
   * Matches if file.isFile
   */   
  def beFile[T <: { def getPath(): String }] = (beAFilePath) ^^ ((_:T).getPath)

  /**
   * Matches if file.isDirectory
   */   
  def beDirectory[T <: { def getPath(): String }] = (beADirectoryPath) ^^ ((_:T).getPath)

  /**
   * Matches if file.getName == name
   */   
  def haveName[T <: { def getPath(): String }](name: String) = (havePathName(name)) ^^ ((_:T).getPath)

  /**
   * Matches if file.getAbsolutePath == path
   */   
  def haveAbsolutePath[T <: { def getPath(): String }](path: String) = (haveAsAbsolutePath(path)) ^^ ((_:T).getPath)
  /**
   * Matches if file.getCanonicalPath == path
   */   
  def haveCanonicalPath[T <: { def getPath(): String }](path: String) = (haveAsCanonicalPath(path)) ^^ ((_:T).getPath)

  /**
   * Matches if file.getParent == path
   */   
  def haveParent[T <: { def getPath(): String }](path: String) = (haveParentPath(path)) ^^ ((_:T).getPath)

  /**
   * Matches if file.list == list
   */   
  def haveList[T <: { def getPath(): String }](list: String) = (listPaths(list)) ^^ ((_:T).getPath)
  
  /** 
   * transforms a string as a Path object to allow matches like:
   * "c:/projects".path must exist
   */
  implicit def asPath(p: String) = Path(p)

  /** 
   * This case class is used to provide the getPath() method,
   * so that all FileMatchers can be used on Strings.
   */
  case class Path(p: String) {
    def path = this
    def getPath(): String = p
  }
}
trait FileBeHaveMatchers { this: FileBaseMatchers =>
  /** 
   * matcher aliases and implicits to use with BeVerb and HaveVerb 
   */
  implicit def toFileResultMatcher[T <: { def getPath(): String }](result: Result[T]) = new FileResultMatcher(result)
  class FileResultMatcher[T <: { def getPath(): String }](result: Result[T]) {
    def hidden = result.matchWith(beHidden)
    def readable = result.matchWith(beReadable)
    def writable = result.matchWith(beWritable)
    def absolute = result.matchWith(beAbsolute)
    def file = result.matchWith(beFile)
    def directory = result.matchWith(beDirectory)
    def name(name: String) = result.matchWith(haveName(name))
    def paths(list: String) = result.matchWith(haveList(list))
    def absolutePath(path: String) = result.matchWith(haveAbsolutePath(path))
    def canonicalPath(path: String) = result.matchWith(haveCanonicalPath(path))
    def parent(path: String) = result.matchWith(haveParent(path))
  }
  def hidden[T <: { def getPath(): String }] = beHidden
  def readable[T <: { def getPath(): String }] = beReadable
  def writable[T <: { def getPath(): String }] = beWritable
  def absolute[T <: { def getPath(): String }] = beAbsolute
  def file[T <: { def getPath(): String }] = beFile
  def directory[T <: { def getPath(): String }] = beDirectory
  def name[T <: { def getPath(): String }](name: String) = haveName(name)
  def paths[T <: { def getPath(): String }](list: String) = haveList(list)
  def absolutePath[T <: { def getPath(): String }](path: String) = haveAbsolutePath(path)
  def canonicalPath[T <: { def getPath(): String }](path: String) = haveCanonicalPath(path)
  def parent[T <: { def getPath(): String }](path: String) = haveParent(path)
}
