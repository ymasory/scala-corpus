package org.ensime.server
import java.io.File
import org.ensime.config.ProjectConfig
import org.ensime.model._
import org.ensime.protocol.ProtocolConversions
import org.ensime.util._
import org.ensime.util.RichFile._
import scala.actors._
import scala.actors.Actor._
import scala.collection.{ Iterable }
import scala.tools.nsc.{ Settings }
import scala.tools.nsc.ast._
import scala.tools.nsc.util.{ OffsetPosition }

case class FullTypeCheckCompleteEvent()
case class CompilerFatalError(e: Throwable)

class Analyzer(val project: Project, val protocol: ProtocolConversions, val config: ProjectConfig)
  extends Actor with RefactoringController {

  private val settings = new Settings(Console.println)
  settings.processArguments(config.compilerArgs, false)
  settings.usejavacp.value = false
  private val reporter = new PresentationReporter()
  protected val scalaCompiler: RichCompilerControl = new RichPresentationCompiler(
    settings, reporter, this, config)
  protected val javaCompiler: JavaCompiler = new JavaCompiler(config)
  protected var awaitingInitialCompile = true

  import scalaCompiler._
  import protocol._

  def act() {
    project ! SendBackgroundMessageEvent("Initializing Analyzer. Please wait...")

    println("Building Java sources...")
    javaCompiler.compileAll()

    println("Building Scala sources...")
    scalaCompiler.askReloadAllFiles()

    loop {
      try {
        receive {
          case AnalyzerShutdownEvent() => {
            javaCompiler.shutdown()
            scalaCompiler.askClearTypeCache()
            scalaCompiler.askShutdown()
            exit('stop)
          }

          // Sent from presentation compiler runner
          // thread, before it dies.
          case CompilerFatalError(e: Throwable) => {
            project ! SendBackgroundMessageEvent("Error analyzing Scala: " + e + ".")
          }

          case FullTypeCheckCompleteEvent() => {
            if (awaitingInitialCompile) {
              project ! AnalyzerReadyEvent()
              awaitingInitialCompile = false
            }
            val result = NoteList('scala, true, reporter.allNotes)
            project ! TypeCheckResultEvent(result)
          }

          case RPCRequestEvent(req: Any, callId: Int) => {
            try {
              if (awaitingInitialCompile) {
                project ! RPCErrorEvent(
                  "Analyzer is not ready! Please wait.", callId)
              } else {
                req match {
                  case ReloadAllReq() => {
                    javaCompiler.reset()
                    javaCompiler.compileAll()
                    val notes = javaCompiler.allNotes
                    project ! TypeCheckResultEvent(NoteList('java, true, notes))

                    scalaCompiler.askReloadAllFiles()
                    project ! RPCResultEvent(toWF(true), callId)
                  }

                  case ReloadFileReq(file: File) => {

                    if (file.getAbsolutePath().endsWith(".java")) {
                      javaCompiler.compileFile(file)
                      val notes = javaCompiler.allNotes
                      project ! TypeCheckResultEvent(NoteList('java, false, notes))
                    }

                    val f = scalaCompiler.sourceFileForPath(file.getAbsolutePath())
                    scalaCompiler.askReloadFile(f)
                    val result = NoteList('scala, false, reporter.allNotes)
                    project ! TypeCheckResultEvent(result)
                    project ! RPCResultEvent(toWF(true), callId)
                  }

                  case req: RefactorPerformReq => {
                    handleRefactorRequest(req, callId)
                  }

                  case req: RefactorExecReq => {
                    handleRefactorExec(req, callId)
                  }

                  case req: RefactorCancelReq => {
                    handleRefactorCancel(req, callId)
                  }

                  case RemoveFileReq(file: File) => {
                    val f = scalaCompiler.sourceFileForPath(file.getAbsolutePath())
                    scalaCompiler.removeUnitOf(f)
                  }

                  case ScopeCompletionReq(file: File, point: Int,
                    prefix: String, constructor: Boolean) => {
                    val f = scalaCompiler.sourceFileForPath(file.getAbsolutePath())
                    val p = new OffsetPosition(f, point)
                    val syms = scalaCompiler.askCompleteSymbolAt(p, prefix, constructor)
                    project ! RPCResultEvent(toWF(syms.map(toWF)), callId)
                  }

                  case TypeCompletionReq(file: File, point: Int, prefix: String) => {
                    val f = scalaCompiler.sourceFileForPath(file.getAbsolutePath())
                    val p = new OffsetPosition(f, point)
                    val members = scalaCompiler.askCompleteMemberAt(p, prefix)
                    project ! RPCResultEvent(toWF(members.map(toWF)), callId)
                  }

                  case PackageMemberCompletionReq(path: String, prefix: String) => {
                    val members = scalaCompiler.askCompletePackageMember(path, prefix)
                    project ! RPCResultEvent(toWF(members.map(toWF)), callId)
                  }

                  case InspectTypeReq(file: File, point: Int) => {
                    val f = scalaCompiler.sourceFileForPath(file.getAbsolutePath())
                    val p = new OffsetPosition(f, point)
                    val result = scalaCompiler.askInspectTypeAt(p) match {
                      case Some(info) => toWF(info)
                      case None => toWF(null)
                    }
                    project ! RPCResultEvent(result, callId)
                  }

                  case InspectTypeByIdReq(id: Int) => {
                    val result = scalaCompiler.askInspectTypeById(id) match {
                      case Some(info) => toWF(info)
                      case None => toWF(null)
                    }
                    project ! RPCResultEvent(result, callId)
                  }

                  case SymbolAtPointReq(file: File, point: Int) => {
                    val f = scalaCompiler.sourceFileForPath(file.getAbsolutePath())
                    val p = new OffsetPosition(f, point)
                    val result = scalaCompiler.askSymbolInfoAt(p) match {
                      case Some(info) => toWF(info)
                      case None => toWF(null)
                    }
                    project ! RPCResultEvent(result, callId)
                  }

                  case InspectPackageByPathReq(path: String) => {
                    val result = scalaCompiler.askPackageByPath(path) match {
                      case Some(info) => toWF(info)
                      case None => toWF(null)
                    }
                    project ! RPCResultEvent(result, callId)
                  }

                  case TypeAtPointReq(file: File, point: Int) => {
                    val f = scalaCompiler.sourceFileForPath(file.getAbsolutePath())
                    val p = new OffsetPosition(f, point)
                    val result = scalaCompiler.askTypeInfoAt(p) match {
                      case Some(info) => toWF(info)
                      case None => toWF(null)
                    }
                    project ! RPCResultEvent(result, callId)
                  }

                  case TypeByIdReq(id: Int) => {
                    val result = scalaCompiler.askTypeInfoById(id) match {
                      case Some(info) => toWF(info)
                      case None => toWF(null)
                    }
                    project ! RPCResultEvent(result, callId)
                  }

                  case TypeByNameReq(name: String) => {
                    val result = scalaCompiler.askTypeInfoByName(name) match {
                      case Some(info) => toWF(info)
                      case None => toWF(null)
                    }
                    project ! RPCResultEvent(result, callId)
                  }

                  case TypeByNameAtPointReq(name: String, file: File, point: Int) => {
                    val f = scalaCompiler.sourceFileForPath(file.getAbsolutePath())
                    val p = new OffsetPosition(f, point)
                    val result = scalaCompiler.askTypeInfoByNameAt(name, p) match {
                      case Some(info) => toWF(info)
                      case None => toWF(null)
                    }
                    project ! RPCResultEvent(result, callId)
                  }

                  case CallCompletionReq(id: Int) => {
                    val result = scalaCompiler.askCallCompletionInfoById(id) match {
                      case Some(info) => toWF(info)
                      case None => toWF(null)
                    }
                    project ! RPCResultEvent(result, callId)
                  }
                }
              }
            } catch {
              case e: Exception => {
                System.err.println("Error handling RPC: " + e + " :\n" +
                  e.getStackTraceString)
                project ! RPCErrorEvent("Error occurred in Analyzer. Check the server log.", callId)
              }
            }
          }
          case other => {
            println("Analyzer: WTF, what's " + other)
          }
        }

      } catch {
        case e: Exception => {
          System.err.println("Error at Compiler message loop: " + e + " :\n" + e.getStackTraceString)
        }
      }
    }
  }

  override def finalize() {
    System.out.println("Finalizing Analyzer actor.")
  }

}

