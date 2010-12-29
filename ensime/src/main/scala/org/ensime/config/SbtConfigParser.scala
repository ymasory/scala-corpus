/*
* Based on code from the sbt project:
* Copyright 2009, 2010  Mark Harrah
*/
package org.ensime.config
import java.io.{ IOException, BufferedReader, File, FileInputStream, InputStreamReader, Reader, StringReader }

object SbtConfigParser extends NotNull {
  type Prop = (String, String)

  def apply(file: File): SbtConfig = {
    try {

      val in = new BufferedReader(
        new InputStreamReader(
          new FileInputStream(file), "UTF-8"))

      def readLine(accum: List[Option[Prop]], index: Int): List[Option[Prop]] = {
        val line = in.readLine()
        if (line eq null) {
          accum.reverse
        } else {
          readLine(parseLine(line, index) ::: accum, index + 1)
        }
      }

      val m = readLine(Nil, 0).flatten.toMap
      val buildScalaVersion = m.get("build.scala.versions").getOrElse("2.8.0")
      new SbtConfig(buildScalaVersion)
    } catch {
      case e: IOException => new SbtConfig("2.8.0")
    }
  }

  private def parseProp(str: String): Option[Prop] = str.split("=", 2) match {
    case Array(name, value) => Some(name.trim, value.trim)
    case x => None
  }

  private def parseLine(content: String, line: Int) = {
    val trimmed = content.trim
    val offset = content.length - trimmed.length

    if (trimmed.isEmpty) {
      Nil
    } else {
      val processed =
        trimmed.charAt(0) match {
          case '#' => None
          case _ => parseProp(trimmed)
        }
      processed :: Nil
    }
  }
}
