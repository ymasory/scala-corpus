package org.ensime.protocol

import java.io._

import scala.actors._ 

import org.ensime.model._
import org.ensime.util._
import org.ensime.config.{ProjectConfig, DebugConfig, ReplConfig}
import org.ensime.debug.{DebugUnit, DebugSourceLinePairs}
import org.ensime.server._


case class IncomingMessageEvent(obj:Any)
case class OutgoingMessageEvent(obj:Any)


trait Protocol extends ProtocolConversions{ 

  /**
  * Read a message from the socket.
  *
  * @param  reader  The reader from which to read the message.
  * @return         The message, in the intermediate format.
  */ 
  def readMessage(reader:Reader):WireFormat

  /**
  * Write a message to the socket.
  *
  * @param  value  The message to write.
  * @param  writer The writer to which to write the message.
  * @return        Void
  */ 
  def writeMessage(value:WireFormat, writer:Writer)


  /**
  * Send a message in wire format to the client. Message
  * will be sent to the outputPeer, and then written to the
  * output socket.
  *
  * @param  o  The message to send.
  * @return    Void
  */ 
  def sendMessage(o:WireFormat){
    peer ! OutgoingMessageEvent(o)
  }


  /**
  * Handle a message from the client. Generally
  * messages encode RPC calls, and will be delegated
  * to the rpcTarget.
  *
  * @param  msg  The message we've received.
  * @return        Void
  */ 
  def handleIncomingMessage(msg:Any)


  /**
  * Send a string to the client editor, to be displayed 
  * to the user. This is to be used for non-critical messaging
  * that the user may choose to ignore.
  *
  * @param  msg  The message to write.
  * @return        Void
  */ 
  def sendBackgroundMessage(msg:String)



  /**
  * Designate an actor that should receive outgoing 
  * messages. 
  * TODO: Perhaps a channel would be more efficient?
  *
  * @param  peer  The Actor.
  * @return        Void
  */ 
  def setOutputActor(peer:Actor)
  protected def peer:Actor

  /**
  * Designate the target to which RPC handling
  * should be delegated.
  *
  * @param  target The RPCTarget instance.
  * @return        Void
  */ 
  def setRPCTarget(target:RPCTarget)


  /**
  * Send a simple RPC Return with a 'true' value.
  * Serves to acknowledge the RPC call when no 
  * other return value is required.
  *
  * @param  callId The id of the RPC call.
  * @return        Void
  */ 
  def sendRPCAckOK(callId:Int)

  /**
  * Send an RPC Return with the given value.
  *
  * @param  value  The value to return.
  * @param  callId The id of the RPC call.
  * @return        Void
  */ 
  def sendRPCReturn(value:WireFormat, callId:Int)

  /**
  * Notify the client that the RPC call could not
  * be handled.
  *
  * @param  value  A message describing the error.
  * @param  callId The id of the failed RPC call.
  * @return        Void
  */ 
  def sendRPCError(msg:String, callId:Int)

  /**
  * Notify the client that a message was received
  * that does not conform to the protocol.
  *
  * @param  packet  The message that failed.
  * @param  condition A string describing the problem.
  * @return        Void
  */ 
  def sendProtocolError(packet:String, condition:String)

  /**
  * Send a structure describing the connection, protocol and
  * server. Probably not necessessary in all clients.
  *
  * @param  callId The id of the failed RPC call.
  * @return        Void
  */ 
  def sendConnectionInfo(callId:Int)

  /**
  * Send a notification that the interactive compiler is ready
  * to process queries. Editor should not allow commands until
  * this notification has been received.
  *
  * @return        Void
  */ 
  def sendCompilerReady()

  /**
  * Send notes describing errors, warnings that the compiler
  * generates. These results are generated asynchronously,
  * and not in response to any single RPC call.
  *
  * @param notes  The notes
  * @return        Void
  */ 
  def sendTypeCheckResult(notes:NoteList)


}

trait ProtocolConversions{
  def toWF(config:ReplConfig):WireFormat
  def toWF(config:DebugConfig):WireFormat
  def toWF(unit:DebugUnit):WireFormat
  def toWF(value:Boolean):WireFormat
  def toWF(value:DebugSourceLinePairs):WireFormat
  def toWF(value:Note):WireFormat
  def toWF(values:Iterable[WireFormat]):WireFormat
  def toWF(value:SymbolInfoLight):WireFormat
  def toWF(value:PackageMemberInfoLight):WireFormat
  def toWF(value:SymbolInfo):WireFormat
  def toWF(value:NamedTypeMemberInfoLight):WireFormat
  def toWF(value:NamedTypeMemberInfo):WireFormat
  def toWF(value:EntityInfo):WireFormat
  def toWF(value:TypeInfo):WireFormat
  def toWF(value:PackageInfo):WireFormat
  def toWF(value:CallCompletionInfo):WireFormat
  def toWF(value:InterfaceInfo):WireFormat
  def toWF(value:TypeInspectInfo):WireFormat

  def toWF(value:RefactorFailure):WireFormat
  def toWF(value:RefactorEffect):WireFormat
  def toWF(value:RefactorResult):WireFormat
  def toWF(value: Null): WireFormat
}
