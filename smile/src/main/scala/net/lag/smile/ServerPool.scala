/*
 * Copyright 2009 Twitter, Inc.
 * Copyright 2009 Robey Pointer <robeypointer@gmail.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License. You may obtain
 * a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.lag.smile

import scala.collection.mutable
import net.lag.configgy.ConfigMap
import net.lag.extensions._
import net.lag.naggati.IoHandlerActorAdapter
import org.apache.mina.core.session.{IdleStatus, IoSession}
import org.apache.mina.filter.codec.{ProtocolCodecFilter, ProtocolEncoder, ProtocolEncoderOutput}
import org.apache.mina.transport.socket.nio.{NioProcessor, NioSocketConnector}
import java.util.concurrent.Executors


/**
 * Pool of memcache server connections, and their shared config.
 *
 * @param trace if true, log all incoming/outgoing data as hexdumps at TRACE level
 */
class ServerPool(trace: Boolean) {

  def this() = this(false)

  var threadPool = Executors.newCachedThreadPool
  var servers: Array[MemcacheConnection] = Array()
  var shouldRebalance = true

  // connections that were recently ejected but might come back:
  private val watchList = new mutable.ListBuffer[MemcacheConnection]

  private var DEFAULT_CONNECT_TIMEOUT = 250
  var retryDelay = 30000
  var readTimeout = 2000
  var connectTimeout = DEFAULT_CONNECT_TIMEOUT
  var maxFailuresBeforeEjection = 3

  // note: this will create one thread per ServerPool
  var connector = new NioSocketConnector(new NioProcessor(threadPool))
  connector.setConnectTimeoutMillis(connectTimeout)
  connector.getSessionConfig.setTcpNoDelay(true)

  // don't always install this.
  if (trace) {
    connector.getFilterChain.addLast("logger", new LoggingFilter)
  }

  connector.getFilterChain.addLast("codec", MemcacheClientDecoder.filter)
  connector.setHandler(new IoHandlerActorAdapter(session => null))

  def liveServers = {
    if (shouldRebalance) {
      servers.filter { !_.isEjected }
    } else {
      servers
    }
  }

  // returns true if one or more ejected connections is ready to be tried again.
  def shouldRecheckEjectedConnections = synchronized {
    if (watchList.exists { !_.isEjected }) {
      scanForEjections()
      true
    } else {
      false
    }
  }

  // scan the server list for ejected connections, and remember them so we can check them later.
  def scanForEjections() {
    synchronized {
      watchList.clear()
      watchList ++= servers.filter { _.isEjected }
    }
  }

  def shutdown() = {
    for (conn <- servers) {
      conn.shutdown
    }
    servers = Array()
    connector.dispose
    threadPool.shutdown
  }

  override def toString() = servers.mkString(", ")
}


object ServerPool {

  val DEFAULT_PORT = 11211
  val DEFAULT_WEIGHT = 1

  /**
   * Make a new MemcacheConnection out of a description string. A description string is:
   * <hostname> [ ":" <port> [ " " <weight> ]]
   * The default port is 11211 and the default weight is 1.
   */
  def makeConnection(desc: String, pool: ServerPool) = {
    val connection = desc.split("[: ]").toList match {
      case hostname :: Nil =>
        new MemcacheConnection(hostname, DEFAULT_PORT, DEFAULT_WEIGHT)
      case hostname :: port :: Nil =>
        new MemcacheConnection(hostname, port.toInt, DEFAULT_WEIGHT)
      case hostname :: port :: weight :: Nil =>
        new MemcacheConnection(hostname, port.toInt, weight.toInt)
      case _ =>
        throw new IllegalArgumentException
    }
    connection.pool = pool
    connection
  }

  /**
   * Make a new ServerPool out of a config block.
   */
  def fromConfig(attr: ConfigMap) = {
    val pool = new ServerPool(attr.getBool("trace", false))
    pool.servers = (for (desc <- attr.getList("servers")) yield makeConnection(desc, pool)).toArray
    if (pool.servers.length == 0) throw new IllegalArgumentException("No servers specified")

    for (n <- attr.getInt("retry_delay_sec")) {
      pool.retryDelay = n * 1000
    }
    for (n <- attr.getInt("read_timeout_msec")) {
      pool.readTimeout = n
    }
    for (n <- attr.getInt("connect_timeout_msec")) {
      pool.connectTimeout = n
      pool.connector.setConnectTimeoutMillis(n)
    }
    for (n <- attr.getInt("max_failures_before_ejection")) {
      pool.maxFailuresBeforeEjection = n
    }
    for (n <- attr.getBool("should_rebalance")) {
      pool.shouldRebalance = n
    }
    pool
  }
}
