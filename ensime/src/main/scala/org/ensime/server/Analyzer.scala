package org.ensime.server
import java.io.File
import org.ensime.config.ProjectConfig
import org.ensime.model._
import org.ensime.protocol.ProtocolConversions
import org.ensime.protocol.ProtocolConst._
import org.ensime.util._
import org.ensime.util.RichFile._
import scala.actors._
import scala.actors.Actor._
import scala.collection.{ Iterable }
import scala.collection.mutable.{ ListBuffer }
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

  println("\nPresentation Compiler settings:")
  println(settings.toString)
  println("")

  private val reporter = new PresentationReporter(new UserMessages {
    override def showError(str: String) {
      project ! SendBackgroundMessageEvent(
        MsgCompilerUnexpectedError, Some(str))
    }
  })

  protected val scalaCompiler: RichCompilerControl = new RichPresentationCompiler(
    settings, reporter, this, config)
  protected val javaCompiler: JavaCompiler = new JavaCompiler(config)
  protected var awaitingInitialCompile = true

  import protocol._
  import scalaCompiler._

  def act() {
    project ! SendBackgroundMessageEvent(
      MsgInitializingAnalyzer, Some("Initializing Analyzer. Please wait..."))

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

          case FullTypeCheckCompleteEvent() => {

            // Block requests while compiler is booting
            if (awaitingInitialCompile) {
              project ! AnalyzerReadyEvent()
              awaitingInitialCompile = false
            }

            val notes = reporter.allNotes

            pendingTypeCheckRequest match {

              // If this compilation was explicitely requested by client...
              case Some((handler, failHandler)) => {
                handler(notes)
                pendingTypeCheckRequest = None
              }

              // Otherwise, use an out-of-band event to notify client
              case None => {
                val result = NoteList(true, notes)
                project ! TypeCheckResultEvent(result)
              }
            }

          }

          case rpcReq@RPCRequestEvent(req: Any, callId: Int) => {
            try {
              if (awaitingInitialCompile) {
                project ! RPCErrorEvent(ErrAnalyzerNotReady,
                  Some("Analyzer is not ready! Please wait."), callId)
              } else {
                req match {

                  case RemoveFileReq(file: File) => {
		    askRemoveDeleted(file)
                  }

                  case ReloadAllReq() => {
                    javaCompiler.reset()
                    javaCompiler.compileAll()
                    val javaNotes = javaCompiler.allNotes
                    waitForScalaTypeCheckResult({ notes =>
                      val nl = NoteList(true, notes ++ javaNotes)
                      project ! RPCResultEvent(toWF(nl), callId)
                    }, { () =>
                      project ! RPCResultEvent(toWF(null), callId)
                    })
                    scalaCompiler.askRemoveAllDeleted()
                    scalaCompiler.askReloadAllFiles()
                  }

                  case ReloadFileReq(file: File) => {
                    val allNotes = new ListBuffer[Note]

                    if (!file.exists()) {
                      project ! RPCErrorEvent(ErrFileDoesNotExist,
                        Some(file.getPath()), callId)
                    }

                    if (file.getAbsolutePath().endsWith(".java")) {
                      javaCompiler.compileFile(file)
                      allNotes ++= javaCompiler.allNotes
                    }

                    waitForScalaTypeCheckResult({ notes =>
                      allNotes ++= notes
                      val nl = NoteList(true, allNotes)
                      project ! RPCResultEvent(toWF(nl), callId)
                    }, { () =>
                      project ! RPCResultEvent(toWF(null), callId)
                    })

                    val f = scalaCompiler.sourceFileForPath(file.getAbsolutePath())
                    scalaCompiler.askReloadFile(f)
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

                  case ScopeCompletionReq(file: File, point: Int,
                    prefix: String, constructor: Boolean) => {
                    val p = pos(file, point)
                    val syms = scalaCompiler.askCompleteSymbolAt(p, prefix, constructor)
                    project ! RPCResultEvent(toWF(syms.map(toWF)), callId)
                  }

                  case TypeCompletionReq(file: File, point: Int, prefix: String) => {
                    val p = pos(file, point)
                    val members = scalaCompiler.askCompleteMemberAt(p, prefix)
                    project ! RPCResultEvent(toWF(members.map(toWF)), callId)
                  }

                  case ImportSuggestionsReq(file: File, point: Int, names: List[String]) => {
                    val p = pos(file, point)
                    val suggestions = scalaCompiler.askImportSuggestions(p, names)
                    project ! RPCResultEvent(toWF(suggestions), callId)
                  }

                  case PackageMemberCompletionReq(path: String, prefix: String) => {
                    val members = scalaCompiler.askCompletePackageMember(path, prefix)
                    project ! RPCResultEvent(toWF(members.map(toWF)), callId)
                  }

                  case InspectTypeReq(file: File, point: Int) => {
                    val p = pos(file, point)
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
                    val p = pos(file, point)
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
                    val p = pos(file, point)
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
                    val p = pos(file, point)
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
                project ! RPCErrorEvent(ErrExceptionInAnalyzer,
                  Some("Error occurred in Analyzer. Check the server log."), callId)
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

  type TypeCheckHandler = (Iterable[Note] => Unit)
  type TypeCheckFailureHandler = (() => Unit)

  private var pendingTypeCheckRequest: Option[(TypeCheckHandler, TypeCheckFailureHandler)] = None

  private def waitForScalaTypeCheckResult(handler: TypeCheckHandler, failHandler: TypeCheckFailureHandler) {
    for ((handler, failHandler) <- pendingTypeCheckRequest) {
      failHandler()
    }
    pendingTypeCheckRequest = Some((handler, failHandler))
  }

  def pos(file: File, offset: Int) = {
    val f = scalaCompiler.sourceFileForPath(file.getAbsolutePath())
    new OffsetPosition(f, offset)
  }

  override def finalize() {
    System.out.println("Finalizing Analyzer actor.")
  }

}

