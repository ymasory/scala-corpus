package org.ensime.util

import scala.collection.mutable.{ ArrayBuffer, SynchronizedMap, LinkedHashMap, HashMap, HashEntry, HashSet }
import scala.tools.nsc.interactive.{ Global, CompilerControl }
import scala.tools.nsc.reporters.{ Reporter, ConsoleReporter }
import scala.tools.nsc.symtab.{ Symbols, Types }
import scala.tools.nsc.util.{ NoPosition, SourceFile, Position, OffsetPosition }
import org.eclipse.jdt.core.compiler.IProblem

case class NoteList(lang: scala.Symbol, full: Boolean, notes: Iterable[Note])

object Note {

  def apply(prob: IProblem) = {
    new Note(
      prob.getOriginatingFileName.mkString,
      prob.getMessage, if (prob.isError) { 2 } else if (prob.isWarning) { 1 } else { 0 },
      prob.getSourceStart,
      prob.getSourceEnd + 1,
      prob.getSourceLineNumber,
      -1
      )
  }
}

class Note(val file: String, val msg: String, val severity: Int, val beg: Int, val end: Int, val line: Int, val col: Int) {

  private val tmp = "" + file + msg + severity + beg + end + line + col;
  override val hashCode = tmp.hashCode

  override def equals(other: Any): Boolean = {
    other match {
      case n: Note => n.hashCode == this.hashCode
      case _ => false
    }
  }

  def friendlySeverity = severity match {
    case 2 => 'error
    case 1 => 'warn
    case 0 => 'info
  }
}
