package org.ensime.server

import java.io._
import java.net.{ ServerSocket, Socket }
import org.ensime.protocol._
import org.ensime.util.WireFormat
import scala.actors._
import scala.actors.Actor._

object Server {
  def main(args: Array[String]): Unit = {
    try {

      System.setProperty("actors.corePoolSize", "5")
      System.setProperty("actors.maxPoolSize", "10")

      val protocol: Protocol = SwankProtocol

      // TODO use a real cmdline parser here
      val portfile = args(0)
      val project: Project = new Project(protocol)
      project.start

      // 0 will cause socket to bind to first available port
      val requestedPort = 0
      val listener = new ServerSocket(requestedPort)
      val actualPort = listener.getLocalPort
      println("Server listening on " + actualPort + "..")
      writePort(portfile, actualPort)
      while (true) {
        try {
          val socket = listener.accept()
          println("Got connection, creating handler...")
          val handler = new SocketHandler(socket, protocol, project)
          handler.start
        } catch {
          case e: IOException =>
            {
              System.err.println("Error in server listen loop: " + e)
            }
        }
      }
      listener.close()
    } catch {
      case e: IOException =>
        {
          System.err.println("Server listen failed: " + e)
          System.exit(-1)
        }
    }
  }

  private def writePort(filename: String, port: Int) {
    val out = new OutputStreamWriter(new FileOutputStream(filename))
    try {
      out.write(port.toString)
      out.flush()
      System.out.println("Wrote port " + port + " to " + filename + ".")
    } catch {
      case e: IOException =>
        {
          System.err.println("Could not write port to " + filename + ". " + e)
          System.exit(-1)
        }
    }
    finally {
      out.close
    }
  }

}


class SocketHandler(socket: Socket, protocol: Protocol, project: Project) extends Actor {

  protocol.setOutputActor(this)

  class SocketReader(socket: Socket, handler: SocketHandler) extends Actor {
    val in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
    def act() {
      var running = true
      try {
        while (running) {
          val msg: WireFormat = protocol.readMessage(in)
          handler ! IncomingMessageEvent(msg)
        }
      } catch {
        case e: IOException =>
          {
            System.err.println("Error in socket reader: " + e)
            exit('error)
          }
      }
    }
  }

  val out = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream()));

  def write(value: WireFormat) {
    try {
      protocol.writeMessage(value, out)
    } catch {
      case e: IOException =>
        {
          System.err.println("Write to client failed: " + e)
          exit('error)
        }
    }
  }

  def act() {
    val reader: SocketReader = new SocketReader(socket, this)
    this.link(reader)
    reader.start
    loop {
      receive {
        case IncomingMessageEvent(value: WireFormat) => {
          project ! IncomingMessageEvent(value)
        }
        case OutgoingMessageEvent(value: WireFormat) => {
          write(value)
        }
        case Exit(_: SocketReader, reason) => exit(reason)
      }
    }
  }

}

