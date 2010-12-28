package com.yuvimasory.flashcards

import java.io.{BufferedReader, BufferedWriter, File, FileReader, FileWriter}

import scala.io.Source

import com.martiansoftware.jsap.{Option => MOption, _}
import com.martiansoftware.jsap.stringparsers._

import StringUtils._

object CLI {

  val ProgramName = "flashup"
  val FlashcardExtension = "flashup"

  val (backs, fronts, pdf, text, input, output) =
    ("backs", "fronts", "pdf", "text", "input", "output")
  lazy val jsap = {
    val jsap = new JSAP()
    val backsSwitch = new Switch(backs)
      .setShortFlag('b')
      .setLongFlag(backs)
    jsap registerParameter backsSwitch
    val frontsSwitch = new Switch(fronts)
      .setShortFlag('f')
      .setLongFlag(fronts)
    jsap registerParameter frontsSwitch
    val pdfSwitch = new Switch(pdf)
      .setLongFlag(pdf)
    jsap registerParameter pdfSwitch
    val textSwitch = new Switch(text)
      .setLongFlag(text)
    jsap registerParameter textSwitch
    val inputOption = new UnflaggedOption(input)
      .setStringParser(FileStringParser.getParser().setMustBeFile(true).setMustExist(true))
      .setRequired(true)
    jsap registerParameter inputOption
    val outputOption = new UnflaggedOption(output)
      .setStringParser(FileStringParser.getParser().setMustBeFile(true).setMustExist(false))
      .setRequired(false)
    jsap registerParameter outputOption
      
    jsap
  }
  
  def usage(config: JSAPResult) = {
    val builder = new java.lang.StringBuilder

    builder append LF
    val iter = config.getErrorMessageIterator()
    while (iter.hasNext()) {
      builder append ("Error: " + iter.next() + LF)
    }
    builder append LF
    builder append ("Usage: java -jar " + ProgramName + ".jar ") + LF
    builder append (jsap.getUsage + LF)
    builder append LF
    builder append ("Options:" + LF)
    builder append ("  -f/--fronts - only generate the fronts of the flashcards (Optional)" + LF)
    builder append ("  -b/--backs - only generate the backs of the flashcards (Optional)" + LF)
    builder append ("  --pdf      - output to PDF format" + LF)
    builder append ("  --text     - output to text format (ignores -b/f, used for debugging)" + LF)
    builder append LF
    builder append ("Examples:" + LF)
    builder append ("  java -jar " + ProgramName + ".jar --pdf path/to/input." + FlashcardExtension)
    builder append LF
    
    builder toString
  }

  def main(args: Array[String]) {
    parseArgs(args) match {
      case Some((outType, pages, inFile, outFile)) => {
        val res = FlashcardParser.parseDoc(inFile)
        res match {
          case Some(doc) => {
            try {
              val translator = OutType.outputMap(outType)
              translator.translate(doc, pages, outFile)
            }
            catch {
              case e: Exception => e.printStackTrace()
            }
          }
          case None => {
            Console.err println ("Could not parse: " + inFile.getAbsolutePath)
            exit(1)
          }
        }
      }
      case None => exit(1)
    }
  }

  private[flashcards] def parseArgs(args: Array[String]): Option[(OutType.Value, Pages.Value, File, File)] = {
    val config = jsap.parse(args)
    config.success match {
      case true => {
        var sides: Pages.Value = Pages.All
        var outType: OutType.Value = OutType.Pdf
        var outFile = config.getFile(output)
        val inFile = config.getFile(input)
        if (config getBoolean(backs))
          sides = Pages.Backs
        if (config getBoolean(fronts))
          sides = Pages.Fronts
        if (config getBoolean(pdf))
          outType = OutType.Pdf
        if (config getBoolean(text))
          outType = OutType.Text

        outFile = outFile match {
          case f: File => f
          case _ => {
            val par = inFile.getParentFile
            val tmp1 = inFile.getName
            val tmp2 = {
              tmp1.split("""\.""") match {
                case a @ Array(_, _*) => a.head.mkString
                case _ => tmp1
              }
            }
            val tmp3 = tmp2 + {
              sides match {
                case Pages.Fronts => "-fronts"
                case Pages.Backs => "-backs"
                case Pages.All => ""
              }
            }

            val newName = tmp3 + ".pdf"
            new File(newName)
          }
        }

        Some(outType, sides, inFile, outFile)
      }
      case false => {
        Console.err println usage(config)
        None
      }
    }
  }
}
object OutType extends Enumeration {
  val Pdf, Text = Value
  val outputMap = Map(Pdf -> PdfTranslator, Text -> TxtTranslator)
}

object Pages extends Enumeration {
  val Fronts, Backs, All = Value
}
