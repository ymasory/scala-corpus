package org.ensime.server

import java.io.File
import org.ensime.config.ProjectConfig
import org.ensime.debug.ProjectDebugInfo
import org.ensime.protocol._
import org.ensime.util._
import scala.actors._
import scala.actors.Actor._
import scala.tools.nsc.{ Settings }
import scala.tools.refactoring.common.Change
import scala.collection.mutable.{ LinkedHashMap }

case class SendBackgroundMessageEvent(code: Int, detail: Option[String])
case class RPCResultEvent(value: WireFormat, callId: Int)
case class RPCErrorEvent(code: Int, detail: Option[String], callId: Int)
case class RPCRequestEvent(req: Any, callId: Int)

case class TypeCheckResultEvent(notes: NoteList)
case class AnalyzerReadyEvent()
case class AnalyzerShutdownEvent()

case class ReloadFileReq(file: File)
case class ReloadAllReq()
case class RemoveFileReq(file: File)
case class ScopeCompletionReq(file: File, point: Int, prefix: String, constructor: Boolean)
case class TypeCompletionReq(file: File, point: Int, prefix: String)
case class ImportSuggestionsReq(file: File, point: Int, names:List[String])
case class PackageMemberCompletionReq(path: String, prefix: String)
case class SymbolAtPointReq(file: File, point: Int)
case class InspectTypeReq(file: File, point: Int)
case class InspectTypeByIdReq(id: Int)
case class InspectPackageByPathReq(path: String)
case class TypeByIdReq(id: Int)
case class TypeByNameReq(name: String)
case class TypeByNameAtPointReq(name: String, file: File, point: Int)
case class CallCompletionReq(id: Int)
case class TypeAtPointReq(file: File, point: Int)

case class AddUndo(summary: String, changes: List[Change])
case class Undo(id: Int, summary: String, changes: Iterable[Change])
case class UndoResult(id: Int, touched: Iterable[File])

class Project(val protocol: Protocol) extends Actor with RPCTarget {

  protocol.setRPCTarget(this)

  protected var analyzer: Actor = actor {}
  protected var builder: Option[Actor] = None
  protected var config: ProjectConfig = ProjectConfig.nullConfig
  protected var debugInfo: Option[ProjectDebugInfo] = None

  private var undoCounter = 0
  private val undos: LinkedHashMap[Int, Undo] = new LinkedHashMap[Int, Undo]

  def act() {
    println("Project waiting for init...")
    loop {
      try {
        receive {
          case SendBackgroundMessageEvent(code: Int, detail: Option[String]) => {
            protocol.sendBackgroundMessage(code, detail)
          }
          case IncomingMessageEvent(msg: WireFormat) => {
            protocol.handleIncomingMessage(msg)
          }
          case msg: AnalyzerReadyEvent => {
            protocol.sendCompilerReady
          }
          case result: TypeCheckResultEvent => {
            protocol.sendTypeCheckResult(result.notes)
          }
          case AddUndo(sum, changes) => {
            addUndo(sum, changes)
          }
          case RPCResultEvent(value, callId) => {
            protocol.sendRPCReturn(value, callId)
          }
          case RPCErrorEvent(code, detail, callId) => {
            protocol.sendRPCError(code, detail, callId)
          }
        }
      } catch {
        case e: Exception => {
          println("Error at Project message loop: " + e + " :\n" + e.getStackTraceString)
        }
      }
    }
  }

  protected def addUndo(sum: String, changes: Iterable[Change]) {
    undoCounter += 1
    undos(undoCounter) = Undo(undoCounter, sum, changes)
  }

  protected def peekUndo(): Either[String, Undo] = {
    undos.lastOption match {
      case Some(u) => Right(u._2)
      case _ => Left("No such undo.")
    }
  }

  protected def execUndo(undoId: Int): Either[String, UndoResult] = {
    undos.get(undoId) match {
      case Some(u) => {
        undos.remove(u.id)
        FileUtils.writeChanges(u.changes) match {
          case Right(touched) => Right(UndoResult(undoId, touched))
          case Left(e) => Left(e.getMessage())
        }
      }
      case _ => Left("No such undo.")
    }
  }

  protected def initProject(conf: ProjectConfig) {
    this.config = conf
    restartCompiler
    shutdownBuilder
    undos.clear
    undoCounter = 0
  }

  protected def restartCompiler() {
    analyzer ! AnalyzerShutdownEvent()
    analyzer = new Analyzer(this, protocol, this.config)
    analyzer.start
  }

  protected def getOrStartBuilder(): Actor = {
    builder match {
      case Some(b) => b
      case None =>
        {
          val b = new IncrementalBuilder(this, protocol, this.config)
          builder = Some(b)
          b.start
          b
        }
    }
  }

  protected def shutdownBuilder() {
    for (b <- builder) {
      b ! BuilderShutdownEvent
    }
    builder = None
  }

}

