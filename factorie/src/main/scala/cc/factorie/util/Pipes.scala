/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */



package cc.factorie.util

/*
 Copyright 2009 David Hall, Daniel Ramage
 
 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 
 
 http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License. 
*/


import java.io.File
import java.io.InputStream
import java.io.OutputStream
import java.lang.Process
import java.lang.ProcessBuilder

import scala.concurrent.ops._

/**
 * Utilities for executing shell scripts and reading from file in
 * a similar way to unix shell piping.  To get started with a global
 * pipes shell, use:
 *
 * import scalanlp.util.Pipes.global._
 *
 * and see examples in the main method.
 *
 * @author dramage
 */
class Pipes {

  // Pipes instance for auto-importing into called contexts.
  implicit val pipes = this;

  //
  // state variables
  //

  protected var _cwd: File = new File(new File("").getAbsolutePath);
  protected var _stdout: OutputStream = java.lang.System.out;
  protected var _stderr: OutputStream = java.lang.System.err;
  protected var _stdin: InputStream = java.lang.System.in;

  //
  // context properties
  //

  /**Returns the default stdout used in this context. */
  def stdout = _stdout;

  /**Sets the default stdout used in this context. */
  def stdout(stream: OutputStream): Unit = _stdout = stream;

  /**Returns the default stderr used in this context. */
  def stderr = _stderr;

  /**Sets the default stderr used in this context. */
  def stderr(stream: OutputStream): Unit = _stderr = stream;

  /**Returns the default stdin used in this context. */
  def stdin = _stdin;

  /**Sets the default stdin used in this context. */
  def stderr(stream: InputStream): Unit = _stdin = stream;

  /**Returns a copy of the pipes context. */
  def copy: Pipes = {
    val _pipes = new Pipes;
    _pipes._cwd = _cwd;
    _pipes._stdout = _stdout;
    _pipes._stderr = _stderr;
    _pipes._stdin = _stdin;
    return _pipes;
  }

  /**
   * Runs the given command (via the system command shell if found)
   * in the current directory.
   */
  def sh(command: String): java.lang.Process = {
    val os = System.getProperty("os.name");
    val pb = new ProcessBuilder().directory(_cwd);

    if (os == "Windows 95" || os == "Windows 98" || os == "Windows ME") {
      pb.command("command.exe", "/C", command);
    } else if (os.startsWith("Windows")) {
      pb.command("cmd.exe", "/C", command);
    } else {
      pb.command("/bin/sh", "-c", command);
    };

    return pb.start();
  }

  /**
   * Returns the current working directory.
   */
  def cwd: File = _cwd;

  /**
   * Changes to the given directory.
   */
  def cd(folder: File) = {
    if (!folder.exists) {
      error("Folder " + folder + " does not exist.");
    } else if (!folder.isDirectory) {
      error("Folder " + folder + " is not a directory");
    } else if (!folder.canRead) {
      error("Cannot access folder " + folder);
    }
    _cwd = folder;
  }

  //
  //  implicit conversions
  //

  implicit def iPipeProcess(process: Process) =
    new PipeProcess(process)(this);

  implicit def iPipeInputStream(stream: InputStream) =
    new PipeInputStream(stream);

  implicit def iPipeInputStream(file: File) =
    new PipeInputStream(iInputStream(file));

  /**
   * Gets a FileInputStream for the given file.  If the filename
   * ends with .gz, automatically wraps the returned stream with
   * a java.util.zip.GZIPInputStream.
   */
  implicit def iInputStream(file: File): InputStream = {
    val fis = new java.io.BufferedInputStream(new java.io.FileInputStream(file));
    if (file.getName.toLowerCase.endsWith(".gz")) {
      return new java.util.zip.GZIPInputStream(fis);
    } else {
      return fis;
    }
  }

  /**
   * Gets a FileOutputStream for the given file.  If the filename
   * ends with .gz, automatically wraps the returned stream with
   * a java.util.zip.GZIPOutputStream.
   */
  implicit def iOutputStream(file: File): OutputStream = {
    val fos = new java.io.BufferedOutputStream(new java.io.FileOutputStream(file));
    if (file.getName.toLowerCase.endsWith(".gz")) {
      return new java.util.zip.GZIPOutputStream(fos);
    } else {
      return fos;
    }
  }

  /**
   * Returns a file with the given name, relative to the current
   * directory (if found and path does not start with
   */
  implicit def File(path: String): File = {
    if (!path.startsWith(java.io.File.separator)) {
      new File(_cwd, path);
    } else {
      new File(path);
    }
  }

  def File(base: java.io.File, path: String) = new File(base, path);

  implicit def iPipeIterator(lines: Iterator[String]) =
    new PipeIterator(lines)(this);

  implicit def iPipeIterator(lines: Iterable[String]) =
    new PipeIterator(lines.iterator)(this);

  private def error(message: String): Unit = {
    throw new PipesException(message);
  }
}

/**
 * To get started with a global pipes shell, use:
 *
 * import scalanlp.util.Pipes.global._
 *
 * And take a look at the example code in the Pipes object's main method.
 */
object Pipes {
  type HasLines = {
    def getLines(): Iterator[String];
  }

  /**A global instance for easy imports */
  val global = new Pipes();

  def apply() = {
    new Pipes();
  }
}

object PipesExample {
  import Pipes.global._;

  def main(argv: Array[String]) {
    sh("echo '(no sleep) prints 1st'") | stdout;
    sh("sleep 1; echo '(sleep 1) prints 2nd'") | stdout;
    sh("echo '(stderr redirect) should show up on stdout' | cat >&2") |& stdout;
    sh("echo '(stderr redirect) should also show up on stdout' | cat >&2") |& sh("cat") | stdout;
    sh("echo '(pipe test line 1) should be printed'; echo '(pipe test line 2) should not be printed'") | sh("grep 1") | stdout;
    sh("echo '(translation test) should sound funny'") | sh("perl -pe 's/(a|e|i|o|u)+/oi/g';") | stdout;
    stdin | sh("egrep '[0-9]'") | stdout;

    (1 to 10).map(_.toString) | stderr;

    for (line <- sh("ls").getLines) {
      println(line.toUpperCase);
    }
  }
}

/**
 * Helper methods for PipeProcess
 *
 * @author dramage
 */
object PipeIO {
  /**
   * Read all bytes from the given input stream to the given output
   * stream, closing the input stream when finished reading.  Does
   * not close the output stream.
   */
  def drain(in: InputStream, out: OutputStream) {
    val buffer = new Array[Byte](1024);

    var numRead = 0;
    do {
      numRead = in.read(buffer, 0, buffer.length);
      if (numRead > 0) {
        // read some bytes
        out.write(buffer, 0, numRead);
      } else if (numRead == 0) {
        // read no bytes, but not yet EOF
        Thread.sleep(100l);
      }
    } while (numRead >= 0)

    in.close();
  }

  /**
   * Reads all lines in the given input stream using Java's
   * BufferedReader.  The returned lines do not have a trailing
   * newline character.
   */
  def readLines(in: InputStream): Iterator[String] = {
    val reader = new java.io.BufferedReader(new java.io.InputStreamReader(in));
    return new Iterator[String]() {
      var line = prepare();

      override def hasNext =
        line != null;

      override def next = {
        val rv = line;
        line = prepare();
        rv;
      }

      def prepare() = {
        val rv = reader.readLine();
        if (rv == null) {
          reader.close();
        }
        rv;
      }
    };
  }
}

/**
 * A richer Process object used for linking together in pipes.
 *
 * @author dramage
 */
class PipeProcess(val process: Process)(implicit pipes: Pipes) {
  import PipeIO._

  /**where stdout and stderr go. */
  protected var out: OutputStream = pipes.stdout;
  protected var err: OutputStream = pipes.stderr;

  def waitFor: Int = process.waitFor();

  /**Close output pipes (on finish) if they are not stdout and stderr */
  private def closePipes() {
    if (out != pipes.stdout && out != pipes.stderr) {
      out.close();
    }
    if (err != pipes.stdout && err != pipes.stderr) {
      err.close();
    }
  }

  def |(next: PipeProcess): PipeProcess = {
    // stdout goes to the next process
    this.out = next.process.getOutputStream;

    spawn {
      val waitForStdin = future {drain(process.getInputStream, out); }
      val waitForStderr = future {drain(process.getErrorStream, err); }

      waitForStdin();
      closePipes();
    }

    return next;
  }

  def |&(next: PipeProcess): PipeProcess = {
    // stdout and stderr both go to the next process
    this.out = next.process.getOutputStream;
    this.err = next.process.getOutputStream;

    spawn {
      val waitForStdin = future {drain(process.getInputStream, out); }
      val waitForStderr = future {drain(process.getErrorStream, err); }

      waitForStdin();
      waitForStderr();
      closePipes();
    }

    return next;
  }

  /**Piping to a process happens immediately via spawning. */
  def |(process: Process): PipeProcess = {
    spawn {
      this | process.getOutputStream;
    }
    return new PipeProcess(process);
  }

  /**Piping to a process happens immediately via spawning. */
  def |&(process: Process): PipeProcess = {
    spawn {
      this |& process.getOutputStream;
    }
    return new PipeProcess(process);
  }

  /**Redirects the given input stream as the source for the process */
  def <(instream: InputStream): Process = {
    spawn {
      val out = process.getOutputStream;
      drain(instream, process.getOutputStream);
      out.close();
    }

    return process;
  }

  /**
   * Redirects output from the process to the given output stream.
   * Blocks until the process completes.
   */
  def |(outstream: OutputStream): Process = {
    this.out = outstream;

    val waitForStdin = future {drain(process.getInputStream, out); }
    val waitForStderr = future {drain(process.getErrorStream, err); }

    waitForStdin();
    closePipes();

    process;
  }

  /**
   * Redirects stdout and stderr from the process to the given output stream.
   * Blocks until the process completes.
   */
  def |&(outstream: OutputStream): Process = {
    this.out = outstream;
    this.err = outstream;

    val waitForStdin = future {drain(process.getInputStream, out); }
    val waitForStderr = future {drain(process.getErrorStream, err); }

    waitForStdin();
    waitForStderr();
    closePipes();

    process;
  }

  /**Pipes to a function that accepts an InputStream. */
  def |[T](func: (InputStream => T)): T =
    func(process.getInputStream);

  /**Reads the lines from this file. */
  def getLines: Iterator[String] =
    readLines(process.getInputStream);
}

/**
 * An alternative richer InputStream that can be piped to an OutputStream,
 * Process, or function.
 *
 * @author dramage
 */
class PipeInputStream(var stream: InputStream) {
  import PipeIO._;

  /**
   * Pipe to an OutputStream.  Returns when all bytes have been
   * written to out.  Does not close out.
   */
  def |(out: OutputStream): Unit =
    drain(stream, out);

  /**
   * Pipe to Process, returning that Process instance.  Returns
   * immediately.  Spawns a background job to write all bytes
   * from the incoming stream to the process.
   */
  def |(process: PipeProcess): Process =
    process < stream;

  /**Pipes to a function that accepts an InputStream. */
  def |[T](func: (InputStream => T)): T =
    func(stream);

  /**Returns all lines in this Stream. */
  def getLines: Iterator[String] =
    readLines(stream);
}

/**
 * A pipeable iterator of Strings, to be written as lines to a stream.
 */
class PipeIterator(lines: Iterator[String])(implicit pipes: Pipes) {
  /**
   * Writes all lines to the given process.  Returns immediately.
   */
  def |(process: PipeProcess): Process = {
    val pipeIn = new java.io.PipedInputStream();
    val pipeOut = new java.io.PipedOutputStream(pipeIn);
    spawn {this | pipeOut; }
    process < pipeIn;
  }

  /**
   * Writes all lines to the given OutputStream, closing it when done
   * if it is not System.out or System.err.
   */
  def |(outstream: OutputStream) = {
    val ps = new java.io.PrintStream(outstream);
    for (line <- lines) {
      ps.println(line);
    }

    if (!(outstream == pipes.stdout || outstream == pipes.stderr)) {
      ps.close;
    }
  }
}

/**
 * Runtime exception thrown by the Pipes framework.
 *
 * @author dramage
 */
class PipesException(message: String) extends RuntimeException(message);
