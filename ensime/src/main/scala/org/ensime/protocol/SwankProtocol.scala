package org.ensime.protocol

import java.io._
import org.ensime.config.{ ProjectConfig, DebugConfig, ReplConfig }
import org.ensime.debug.{ DebugUnit, DebugSourceLinePairs }
import org.ensime.model._
import org.ensime.server._
import org.ensime.util._
import org.ensime.util.SExp._
import scala.actors._
import scala.tools.nsc.util.{ Position }
import scala.tools.refactoring.common.Change
import scala.util.parsing.input

object SwankProtocol extends SwankProtocol {}

trait SwankProtocol extends Protocol {

  import SwankProtocol._
  import ProtocolConst._

  val PROTOCOL_VERSION: String = "0.0.1"

  val SERVER_NAME: String = "ENSIMEserver"

  private var outPeer: Actor = null;
  private var rpcTarget: RPCTarget = null;

  def peer = outPeer

  def setOutputActor(peer: Actor) { outPeer = peer }

  def setRPCTarget(target: RPCTarget) { this.rpcTarget = target }

  // Handle reading / writing of messages

  def writeMessage(value: WireFormat, out: Writer) {
    val data: String = value.toWireString
    val header: String = String.format("%06x", int2Integer(data.length))
    val msg = header + data
    println("Writing: " + msg)
    out.write(msg)
    out.flush()
  }

  private def fillArray(in: java.io.Reader, a: Array[Char]) {
    var n = 0
    var l = a.length
    var charsRead = 0;
    while (n < l) {
      charsRead = in.read(a, n, l - n)
      if (charsRead == -1) {
        throw new EOFException("End of file reached in socket reader.");
      } else {
        n += charsRead
      }
    }
  }

  private val headerBuf = new Array[Char](6);

  def readMessage(in: java.io.Reader): WireFormat = {
    fillArray(in, headerBuf)
    val msglen = Integer.valueOf(new String(headerBuf), 16).intValue()
    if (msglen > 0) {
      //TODO allocating a new array each time is inefficient!
      val buf: Array[Char] = new Array[Char](msglen);
      fillArray(in, buf)
      SExp.read(new input.CharArrayReader(buf))
    } else {
      throw new IllegalStateException("Empty message read from socket!")
    }
  }

  def sendBackgroundMessage(code: Int, detail: Option[String]) {
    sendMessage(SExp(
      key(":background-message"),
      code,
      detail.map(strToSExp).getOrElse(NilAtom())))
  }

  def handleIncomingMessage(msg: Any) {
    msg match {
      case sexp: SExp => handleMessageForm(sexp)
      case _ => System.err.println("WTF: Unexpected message: " + msg)
    }
  }

  private def handleMessageForm(sexp: SExp) {
    sexp match {
      case SExpList(KeywordAtom(":swank-rpc") :: form :: IntAtom(callId) :: rest) => {
        handleEmacsRex(form, callId)
      }
      case _ => {
        sendProtocolError(ErrUnrecognizedForm, Some(sexp.toReadableString))
      }
    }
  }

  private def handleEmacsRex(form: SExp, callId: Int) {
    form match {
      case SExpList(SymbolAtom(name) :: rest) => {
        try {
          handleRPCRequest(name, form, callId)
        } catch {
          case e: Throwable =>
            {
            e.printStackTrace(System.err)
            sendRPCError(ErrExceptionInRPC, Some(e.getMessage), callId)
          }
        }
      }
      case _ => {
        sendRPCError(
          ErrMalformedRPC,
          Some("Expecting leading symbol in: " + form),
          callId)
      }
    }
  }

  private def handleRPCRequest(callType: String, form: SExp, callId: Int) {

    println("\nHandling RPC: " + form)

    def oops = sendRPCError(ErrMalformedRPC, Some("Malformed " + callType + " call: " + form), callId)

    callType match {
      case "swank:connection-info" => {
        sendConnectionInfo(callId)
      }
      case "swank:init-project" => {
        form match {
          case SExpList(head ::(conf: SExpList) :: body) => {
            val config = ProjectConfig.fromSExp(conf)
            rpcTarget.rpcInitProject(config, callId)
          }
          case _ => oops
        }
      }
      case "swank:peek-undo" => {
        rpcTarget.rpcPeekUndo(callId)
      }
      case "swank:exec-undo" => {
        form match {
          case SExpList(head ::(IntAtom(id)) :: body) => {
            rpcTarget.rpcExecUndo(id, callId)
          }
          case _ => oops
        }
      }
      case "swank:repl-config" => {
        rpcTarget.rpcReplConfig(callId)
      }
      case "swank:builder-init" => {
        rpcTarget.rpcBuilderInit(callId)
      }
      case "swank:builder-add-files" => {
        form match {
          case SExpList(head :: SExpList(filenames) :: body) => {
            val files = filenames.map(_.toString)
            rpcTarget.rpcBuilderAddFiles(files, callId)
          }
          case _ => oops
        }
      }
      case "swank:builder-update-files" => {
        form match {
          case SExpList(head :: SExpList(filenames) :: body) => {
            val files = filenames.map(_.toString)
            rpcTarget.rpcBuilderUpdateFiles(files, callId)
          }
          case _ => oops
        }
      }
      case "swank:builder-remove-files" => {
        form match {
          case SExpList(head :: SExpList(filenames) :: body) => {
            val files = filenames.map(_.toString)
            rpcTarget.rpcBuilderRemoveFiles(files, callId)
          }
          case _ => oops
        }
      }
      case "swank:debug-config" => {
        rpcTarget.rpcDebugConfig(callId)
      }
      case "swank:debug-unit-info" => {
        form match {
          case SExpList(head :: StringAtom(sourceName) :: IntAtom(line) :: StringAtom(packPrefix) :: body) => {
            rpcTarget.rpcDebugUnitInfo(sourceName, line, packPrefix, callId)
          }
          case _ => oops
        }
      }

      case "swank:debug-class-locs-to-source-locs" => {
        form match {
          case SExpList(head :: SExpList(pairs) :: body) => {
            val nameLinePairs = pairs.flatMap {
              case SExpList((classname: StringAtom) ::(line: IntAtom) :: body) => {
                Some(classname.toString, line.value)
              }
              case _ => Some("", -1)
            }
            rpcTarget.rpcDebugClassLocsToSourceLocs(nameLinePairs, callId)
          }
          case _ => oops
        }
      }
      case "swank:remove-file" => {
        form match {
          case SExpList(head :: StringAtom(file) :: body) => {
            rpcTarget.rpcRemoveFile(file, callId)
          }
          case _ => oops
        }
      }
      case "swank:typecheck-file" => {
        form match {
          case SExpList(head :: StringAtom(file) :: body) => {
            rpcTarget.rpcTypecheckFile(file, callId)
          }
          case _ => oops
        }
      }
      case "swank:typecheck-all" => {
        rpcTarget.rpcTypecheckAll(callId)
      }
      case "swank:scope-completion" => {
        form match {
          case SExpList(head :: StringAtom(file) :: IntAtom(point) :: StringAtom(prefix) :: BooleanAtom(constructor) :: body) => {
            rpcTarget.rpcScopeCompletion(file, point, prefix, constructor, callId)
          }
          case _ => oops
        }
      }
      case "swank:type-completion" => {
        form match {
          case SExpList(head :: StringAtom(file) :: IntAtom(point) :: StringAtom(prefix) :: body) => {
            rpcTarget.rpcTypeCompletion(file, point, prefix, callId)
          }
          case _ => oops
        }
      }
      case "swank:import-suggestions" => {
        form match {
          case SExpList(head :: StringAtom(file) :: IntAtom(point) :: SExpList(names) :: body) => {
            rpcTarget.rpcImportSuggestions(file, point,
              names.map(_.toString).toList, callId)
          }
          case _ => oops
        }
      }
      case "swank:package-member-completion" => {
        form match {
          case SExpList(head :: StringAtom(path) :: StringAtom(prefix) :: body) => {
            rpcTarget.rpcPackageMemberCompletion(path, prefix, callId)
          }
          case _ => oops
        }
      }
      case "swank:inspect-type-at-point" => {
        form match {
          case SExpList(head :: StringAtom(file) :: IntAtom(point) :: body) => {
            rpcTarget.rpcInspectTypeAtPoint(file, point, callId)
          }
          case _ => oops
        }
      }
      case "swank:inspect-type-by-id" => {
        form match {
          case SExpList(head :: IntAtom(id) :: body) => {
            rpcTarget.rpcInspectTypeById(id, callId)
          }
          case _ => oops
        }
      }
      case "swank:symbol-at-point" => {
        form match {
          case SExpList(head :: StringAtom(file) :: IntAtom(point) :: body) => {
            rpcTarget.rpcSymbolAtPoint(file, point, callId)
          }
          case _ => oops
        }
      }
      case "swank:type-by-id" => {
        form match {
          case SExpList(head :: IntAtom(id) :: body) => {
            rpcTarget.rpcTypeById(id, callId)
          }
          case _ => oops
        }
      }
      case "swank:type-by-name" => {
        form match {
          case SExpList(head :: StringAtom(name) :: body) => {
            rpcTarget.rpcTypeByName(name, callId)
          }
          case _ => oops
        }
      }
      case "swank:type-by-name-at-point" => {
        form match {
          case SExpList(head :: StringAtom(name) :: StringAtom(file) :: IntAtom(point) :: body) => {
            rpcTarget.rpcTypeByNameAtPoint(name, file, point, callId)
          }
          case _ => oops
        }
      }
      case "swank:call-completion" => {
        form match {
          case SExpList(head :: IntAtom(id) :: body) => {
            rpcTarget.rpcCallCompletion(id, callId)
          }
          case _ => oops
        }
      }
      case "swank:type-at-point" => {
        form match {
          case SExpList(head :: StringAtom(file) :: IntAtom(point) :: body) => {
            rpcTarget.rpcTypeAtPoint(file, point, callId)
          }
          case _ => oops
        }
      }
      case "swank:inspect-package-by-path" => {
        form match {
          case SExpList(head :: StringAtom(path) :: body) => {
            rpcTarget.rpcInspectPackageByPath(path, callId)
          }
          case _ => oops
        }
      }

      case "swank:perform-refactor" => {
        form match {
          case SExpList(head :: IntAtom(procId) :: SymbolAtom(tpe) ::(params: SExp) :: body) => {
            rpcTarget.rpcPerformRefactor(Symbol(tpe), procId,
              listOrEmpty(params).toSymbolMap, callId)
          }
          case _ => oops
        }
      }

      case "swank:exec-refactor" => {
        form match {
          case SExpList(head :: IntAtom(procId) :: SymbolAtom(tpe) :: body) => {
            rpcTarget.rpcExecRefactor(Symbol(tpe), procId, callId)
          }
          case _ => oops
        }
      }

      case "swank:cancel-refactor" => {
        form match {
          case SExpList(head :: IntAtom(procId) :: body) => {
            rpcTarget.rpcCancelRefactor(procId, callId)
          }
          case _ => oops
        }
      }

      case "swank:format-source" => {
        form match {
          case SExpList(head :: SExpList(filenames) :: body) => {
            val files = filenames.map(_.toString)
            rpcTarget.rpcFormatFiles(files, callId)
          }
          case _ => oops
        }
      }

      case other => {
        sendRPCError(
          ErrUnrecognizedRPC,
          Some("Unknown :swank-rpc call: " + other),
          callId)
      }
    }
  }

  def listOrEmpty(list: SExp): SExpList = {
    list match {
      case l: SExpList => l
      case _ => SExpList(List())
    }
  }

  def sendRPCAckOK(callId: Int) {
    sendRPCReturn(true, callId)
  }

  def sendRPCReturn(value: WireFormat, callId: Int) {
    value match {
      case sexp: SExp =>
        {
        sendMessage(SExp(
          key(":return"),
          SExp(key(":ok"), sexp),
          callId))
      }
      case _ => throw new IllegalStateException("Not a SExp: " + value)
    }
  }

  def sendRPCError(code: Int, detail: Option[String], callId: Int) {
    sendMessage(SExp(
      key(":return"),
      SExp(key(":abort"),
        code,
        detail.map(strToSExp).getOrElse(NilAtom())),
      callId))
  }

  def sendProtocolError(code: Int, detail: Option[String]) {
    sendMessage(
      SExp(
        key(":reader-error"),
        code,
        detail.map(strToSExp).getOrElse(NilAtom())))
  }

  /*
  * A sexp describing the server configuration, per the Swank standard.
  */
  def sendConnectionInfo(callId: Int) = {
    val info = SExp(
      key(":pid"), 'nil,
      key(":server-implementation"),
      SExp(
        key(":name"), SERVER_NAME),
      key(":machine"), 'nil,
      key(":features"), 'nil,
      key(":version"), PROTOCOL_VERSION)
    sendRPCReturn(info, callId)
  }

  def sendCompilerReady() = sendMessage(SExp(key(":compiler-ready"), true))

  def sendTypeCheckResult(notelist: NoteList) = {
    sendMessage(SExp(key(":typecheck-result"), toWF(notelist)))
  }

  object SExpConversion {

    implicit def posToSExp(pos: Position): SExp = {
      if (pos.isDefined) {
        SExp.propList((":file", pos.source.path), (":offset", pos.point))
      } else {
        'nil
      }
    }

  }

  import SExpConversion._

  def toWF(config: ProjectConfig): SExp = {
    SExp(
      key(":project-name"), config.name.map(StringAtom).getOrElse('nil),
      key(":source-roots"), SExp(config.sourceRoots.map { f => StringAtom(f.getPath) }))
  }

  def toWF(config: ReplConfig): SExp = {
    SExp.propList((":classpath", strToSExp(config.classpath)))
  }

  def toWF(config: DebugConfig): SExp = {
    SExp.propList(
      (":classpath", strToSExp(config.classpath)),
      (":sourcepath", strToSExp(config.sourcepath)))
  }

  def toWF(unit: DebugUnit): SExp = {
    SExp.propList(
      (":full-name", strToSExp(unit.classQualName)),
      (":package", strToSExp(unit.packageName)),
      (":start-line", intToSExp(unit.startLine)),
      (":end-line", intToSExp(unit.endLine)))
  }

  def toWF(value: Boolean): SExp = {
    if (value) TruthAtom()
    else NilAtom()
  }

  def toWF(value: Null): SExp = {
    NilAtom()
  }

  def toWF(value: String): SExp = {
    StringAtom(value)
  }

  def toWF(value: DebugSourceLinePairs): SExp = {
    SExpList(value.pairs.map { p => SExp(p._1, p._2) })
  }

  def toWF(note: Note): SExp = {
    SExp(
      key(":severity"), note.friendlySeverity,
      key(":msg"), note.msg,
      key(":beg"), note.beg,
      key(":end"), note.end,
      key(":line"), note.line,
      key(":col"), note.col,
      key(":file"), note.file)
  }

  def toWF(notelist: NoteList): SExp = {
    val NoteList(isFull, notes) = notelist
    SExp(
      key(":is-full"),
      toWF(isFull),
      key(":notes"),
      SExpList(notes.map(toWF)))
  }

  def toWF(values: Iterable[WireFormat]): SExp = {
    SExpList(values.map(ea => ea.asInstanceOf[SExp]))
  }

  def toWF(value: SymbolInfoLight): SExp = {
    SExp.propList(
      (":name", value.name),
      (":type-sig", value.tpeSig),
      (":type-id", value.tpeId),
      (":is-callable", value.isCallable))
  }

  def toWF(value: PackageMemberInfoLight): SExp = {
    SExp(key(":name"), value.name)
  }

  def toWF(value: SymbolInfo): SExp = {
    SExp.propList(
      (":name", value.name),
      (":type", toWF(value.tpe)),
      (":decl-pos", value.declPos),
      (":is-callable", value.isCallable))
  }

  def toWF(value: NamedTypeMemberInfoLight): SExp = {
    SExp.propList(
      (":name", value.name),
      (":type-sig", value.tpeSig),
      (":type-id", value.tpeId),
      (":is-callable", value.isCallable))
  }

  def toWF(value: NamedTypeMemberInfo): SExp = {
    SExp.propList(
      (":name", value.name),
      (":type", toWF(value.tpe)),
      (":pos", value.pos),
      (":decl-as", value.declaredAs))
  }

  def toWF(value: EntityInfo): SExp = {
    value match {
      case value: PackageInfo => toWF(value)
      case value: TypeInfo => toWF(value)
      case value: NamedTypeMemberInfo => toWF(value)
      case value: NamedTypeMemberInfoLight => toWF(value)
      case value => throw new IllegalStateException("Unknown EntityInfo: " + value)
    }
  }

  def toWF(value: TypeInfo): SExp = {
    value match {
      case value: ArrowTypeInfo =>
        {
        SExp.propList(
          (":name", value.name),
          (":type-id", value.id),
          (":arrow-type", true),
          (":result-type", toWF(value.resultType)),
          (":param-sections", SExp(value.paramSections.map(toWF))))
      }
      case value: TypeInfo =>
        {
        SExp.propList((":name", value.name),
          (":type-id", value.id),
          (":full-name", value.fullName),
          (":decl-as", value.declaredAs),
          (":type-args", SExp(value.args.map(toWF))),
          (":members", SExp(value.members.map(toWF))),
          (":pos", value.pos),
          (":outer-type-id", value.outerTypeId.map(intToSExp).getOrElse('nil)))
      }
      case value => throw new IllegalStateException("Unknown TypeInfo: " + value)
    }
  }

  def toWF(value: PackageInfo): SExp = {
    SExp.propList((":name", value.name),
      (":info-type", 'package),
      (":full-name", value.fullname),
      (":members", SExpList(value.members.map(toWF))))
  }

  def toWF(value: CallCompletionInfo): SExp = {
    SExp.propList(
      (":result-type", toWF(value.resultType)),
      (":param-sections", SExp(value.paramSections.map(toWF))))
  }

  def toWF(value: ParamSectionInfo): SExp = {
    SExp.propList(
      (":params", SExp(value.params.map {
        case (nm, tp) => SExp(nm, toWF(tp))
      })),
      (":is-implicit", value.isImplicit))

  }

  def toWF(value: InterfaceInfo): SExp = {
    SExp.propList(
      (":type", toWF(value.tpe)),
      (":via-view", value.viaView.map(strToSExp).getOrElse('nil)))
  }

  def toWF(value: TypeInspectInfo): SExp = {
    SExp.propList(
      (":type", toWF(value.tpe)),
      (":info-type", 'typeInspect),
      (":companion-id", value.companionId match {
        case Some(id) => id
        case None => 'nil
      }), (":interfaces", SExp(value.supers.map(toWF))))
  }

  def toWF(value: RefactorFailure): SExp = {
    SExp.propList(
      (":procedure-id", value.procedureId),
      (":status", 'failure),
      (":reason", value.message))
  }

  def toWF(value: RefactorEffect): SExp = {
    SExp.propList(
      (":procedure-id", value.procedureId),
      (":refactor-type", value.refactorType),
      (":status", 'success),
      (":changes", SExpList(value.changes.map(changeToWF))))
  }

  def toWF(value: RefactorResult): SExp = {
    SExp.propList(
      (":procedure-id", value.procedureId),
      (":refactor-type", value.refactorType),
      (":touched-files", SExpList(value.touched.map(f => strToSExp(f.getAbsolutePath)))))
  }

  def toWF(value: ImportSuggestions): SExp = {
    SExpList(value.symLists.map { l => SExpList(l.map(toWF)) })
  }

  def toWF(value: Undo): SExp = {
    SExp.propList(
      (":id", value.id),
      (":changes", SExpList(value.changes.map(changeToWF))),
      (":summary", value.summary))
  }

  def toWF(value: UndoResult): SExp = {
    SExp.propList(
      (":id", value.id),
      (":touched-files", SExpList(value.touched.map(f => strToSExp(f.getAbsolutePath)))))
  }

  private def changeToWF(ch: Change): SExp = {
    SExp.propList(
      (":file", ch.file.path),
      (":text", ch.text),
      (":from", ch.from),
      (":to", ch.to))
  }

}
