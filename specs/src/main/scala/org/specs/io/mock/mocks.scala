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
package org.specs.io.mock

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue
import scala.collection.mutable.HashMap
import org.specs.io.Output
import org.specs.io.FileSystem

/**
 * The MockFileSystem trait mocks the FileSystem by storing a Map[path, content] representing the content of the FileSystem
 */
trait MockFileSystem extends FileSystem {

  /** default extension which can be used when creating default file names */
  var defaultExtension = ""

  /** this map associates some file paths with file contents */
  var files = new HashMap[String, String]

  /** this list registers created directories*/
  var createdDirs = List[String]()

  /** this map associates some file paths with children paths */
  var children = new HashMap[String, ListBuffer[String]]

  /** this list stores readable files */
  var readableFiles = List[String]()

  /** this list stores writable files */
  var writableFiles = List[String]()

  /** @return the content of a file corresponding to a given path */
  override def readFile(path: String) = files(path)

  /** @return all file paths */
  override def filePaths(path: String) = files.keySet.toList

  /** adds a new file to the FileSystem. The file path will be a default one */
  def addFile(content: String): Unit = addFile(defaultFilePath, content)

  /** adds a new file to the FileSystem with a specific file path */
  def addFile(path: String, content: String) = {
    files += Pair(path, content)
    readableFiles ::= path
    writableFiles ::= path
  }
  /** adds a new child to a given file */
  def addChild(parent: String, child: String): Unit = {
    children.get(parent) match {
      case Some(l) => () 
      case None => children.put(parent, new ListBuffer)
    }
    children.get(parent).get += child
  }
  /** sets a file as readable */
  def setReadable(path: String) = if (!canRead(path)) (readableFiles ::= path)
  /** sets a file as writable */
  def setWritable(path: String) = if (!canWrite(path)) (writableFiles ::= path)

  /** sets a file as not readable */
  def setNotReadable(path: String) = readableFiles = readableFiles.filterNot(_ == path)
  /** sets a file as not writable */
  def setNotWritable(path: String) = writableFiles = writableFiles.filterNot(_ == path)

  /** overrides the canRead definition checking in the readableFiles list */
  override def canRead(path: String) = readableFiles.exists(_ == path)
  /** overrides the canWrite definition checking in the writableFiles list */
  override def canWrite(path: String) = writableFiles.exists(_ == path)
  /** overrides the isAbsolute definition checking if it starts with / (partial definition) */
  override def isAbsolute(path: String) = path.startsWith("/") || path.startsWith("\\")
  /** overrides the isHidden definition checking if it starts with . (partial definition) */
  override def isHidden(path: String) = path.startsWith(".")
  /** overrides the isFile definition checking if it doesn't end with / (partial definition) */
  override def isFile(path: String) = path.matches(".*\\..*")
  /** overrides the isDirectory definition checking if it ends with / (partial definition) */
  override def isDirectory(path: String) = !isFile(path)
  /** overrides the listFiles definition */
  override def listFiles(path: String) = children.get(path.replaceAll("\\\\", "/")).map(_.toList).getOrElse(List[String]())

  /** @return a default file path. All default file paths will be different from each other */
  def defaultFilePath = "name" + files.size + defaultExtension

  /** creates a file with the specified path but an empty content */
  override def createFile(path: String) = {files += (path -> ""); true}

  /** create a new directory */
  override def mkdirs(path: String) = { createdDirs = path :: createdDirs; true }
  /** create a new directory */
  override def createDir(path: String) = mkdirs(path)

  /** @returns a mock FileWriter for a specific path */
  override def getWriter(path: String) = MockFileWriter(path)

  case class MockFileWriter(path: String) extends MockWriter {
    override def write(m: String): Unit = files(path) = files.getOrElse(path, "") + m
  }
  /** removes all specified files */
  def reset: this.type = {
    files = new HashMap[String, String]
    children = new HashMap[String, ListBuffer[String]]
    readableFiles = Nil
    writableFiles = Nil
    this
  }
  
  override def exists(path: String) = files.contains(path)
  
  override def inputStream(filePath: String) = new java.io.InputStream {
	val reader = new java.io.StringReader(readFile(filePath))
	def read() = {
	  reader.read()
	}
  }
}

/**
 * The MockOutput trait catches all messages printed with the Output trait
 */
trait MockOutput extends Output {

  /** list of messages representing the output */
  private val someMessages : Queue[String] = new Queue[String]
  def messages: List[String] = someMessages.toList
  
  /** clear the list of current messages */
  def clearMessages = someMessages.clear
  
  /** adds <code>m.toString</code> to a list of messages */
  override def println(m : Any) : Unit = someMessages += m.toString

  /** prints several objects according to a format string (see <code>Console.printf</code>) */
  override def printf(format: String, args: Any*) = someMessages += (format + ":" + args)

  /** doesn't flush */
  override def flush = ()
  
  /** doesn't print anything */
  override def printStackTrace(t: Throwable) = t.getStackTrace.foreach { someMessages += _.toString }
}

/**
 * The MockWriter writes all the content written with the Writer interface to the <code>messages: Queue[String]</code> attribute.
 */
trait MockWriter extends java.io.Writer {

  /** list of messages representing the output */
  private val someMessages : Queue[String] = new Queue[String]
  def messages: List[String] = someMessages.toList
  
  /** is the Writer closed? */
  var closed = false
  override def write(m: String) : Unit = someMessages += m

  /** closes the Writer */
  override def close = closed = true

  /** flushes the Writer */
  override def flush = {}

  /** overrides the write(a: Array[Char], b: Int, c: Int) method to do nothing */
  override def write(a: Array[Char], b: Int, c: Int) = {}
}
