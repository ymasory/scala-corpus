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
import com.twitter.actors.{Future, Futures}
import net.lag.configgy.ConfigMap
import net.lag.logging.Logger
import net.lag.extensions._

/**
 * A memcache client that talks to a pool of servers, and gets and sets values using a codec
 * to convert them to/from binary strings.
 *
 * Convenience factory methods exist on the `MemcacheClient` object.
 */
class MemcacheClient[T](locator: NodeLocator, codec: MemcacheCodec[T]) {
  private val log = Logger.get(getClass.getName)

  private var pool: ServerPool = null
  var namespace: Option[String] = None

  val MAX_KEY_SIZE = 250


  def setPool(pool: ServerPool) = {
    this.pool = pool
    locator.setPool(pool)
  }

  /**
   * Shutdown this memcache client instance. This is necessary in order to stop the connection
   * actors and terminate mina threads.
   */
  def shutdown() = {
    pool.shutdown
    pool = null
  }

  /**
   * Get the stats of the memcache cluster.
   */
  @throws(classOf[MemcacheServerException])
  def stats(): List[(String, Map[String, String])] = {
    var stats = new mutable.ListBuffer[(String, Map[String, String])]
    for (node <- pool.servers) {
      stats += ((node.hostname, node.stats))
    }
    stats.toList
  }

  override def toString() = {
    "<MemcacheClient locator=%s servers=%s>".format(locator, pool)
  }

  /**
   * Return the list of memcache server connections.
   */
  def servers = pool.servers


  /**
   * Get an item from the memcache cluster as an array of bytes.
   */
  @throws(classOf[MemcacheServerException])
  def getData(key: String): Option[Array[Byte]] = {
    withNode(key) { (node, key) =>
      node.get(key) match {
        case None => None
        case Some(v) => Some(v.data)
      }
    }
  }

  /**
   * Get an item from the memcache cluster and decode it using the default
   * codec.
   */
  @throws(classOf[MemcacheServerException])
  def get(key: String): Option[T] = {
    getData(key) match {
      case None => None
      case Some(data) => Some(codec.decode(data))
    }
  }

  /**
   * Get an item from the memcache cluster and decode it using a specific
   * codec.
   */
  @throws(classOf[MemcacheServerException])
  def get[A](key: String, codec: MemcacheCodec[A]): Option[A] = {
    getData(key) match {
      case None => None
      case Some(data) => Some(codec.decode(data))
    }
  }

  /**
   * Get a list of items from the memcache cluster and return the ones that
   * exist in a map associating keys to arrays of bytes. If some of the keys
   * map to different memcache servers, the servers will be contacted
   * concurrently. A multi-get will be used on each server to get every item
   * from that server at once.
   */
  @throws(classOf[MemcacheServerException])
  def getData(keys: Array[String]): Map[String, Array[Byte]] = {
    val keyMap = new mutable.HashMap[String, String]
    val nodeKeys = new mutable.HashMap[MemcacheConnection, mutable.ListBuffer[String]]
    for (key <- keys) {
      val (node, realKey) = nodeForKey(key)
      keyMap(realKey) = key
      nodeKeys.getOrElseUpdate(node, new mutable.ListBuffer[String]) += realKey
    }

    val futures: Iterable[Future[Map[String, MemcacheResponse.Value]]] = for ((node, keyList) <- nodeKeys) yield BulletProofFuture.future {
      withNode(node) {
        // had to give the compiler more info here in 2.8.0
        val rv: Map[String, MemcacheResponse.Value] = try {
          node.get(keyList.toArray)
        } catch {
          case e: Exception => {
            log.error("Exception contacting %s: %s", node, e)
            Map.empty
          }
        }
        rv
      }
    }
    Map.empty ++ (for (future <- futures; (key, value) <- future()) yield (keyMap(key), value.data))
  }

  /**
   * Get a list of items from the memcache cluster and return the ones that
   * exist in a map associating keys to items decoded using the default
   * codec. Parallel fetching is used, just as in <code>getData</code>.
   */
  @throws(classOf[MemcacheServerException])
  def get(keys: Array[String]): Map[String, T] = {
    Map.empty ++ (for ((key, data) <- getData(keys).elements) yield (key, codec.decode(data)))
  }

  /**
   * Get a list of items from the memcache cluster and return the ones that
   * exist in a map associating keys to items decoded using the codec given.
   * Parallel fetching is used, just as in <code>getData</code>.
   */
  @throws(classOf[MemcacheServerException])
  def get[A](keys: Array[String], codec: MemcacheCodec[A]): Map[String, A] = {
    Map.empty ++ (for ((key, data) <- getData(keys).elements) yield (key, codec.decode(data)))
  }

  /**
   * Set an item in memcache as a byte array.
   *
   * @param flogs arbitrary flags to be saved by the server (they have no intrinsic significance)
   * @param expiry absolute or relative time, in seconds or epoch itme, when this item should
   *   expire from the cache (0 = never)
   */
  @throws(classOf[MemcacheServerException])
  def setData(key: String, value: Array[Byte], flags: Int, expiry: Int): Boolean = {
    withNode(key) { (node, rkey) =>
      node.set(rkey, value, flags, expiry)
    }
  }

  /**
   * Set an item in memcache as a byte array.
   * The item's expiration will be "never".
   */
  @throws(classOf[MemcacheServerException])
  def setData(key: String, value: Array[Byte]): Boolean = setData(key, value, 0, 0)

  /**
   * Encode an item using the default codec and set it into the memcache pool.
   *
   * @param flogs arbitrary flags to be saved by the server (they have no intrinsic significance)
   * @param expiry absolute or relative time, in seconds or epoch itme, when this item should
   *   expire from the cache (0 = never)
   */
  @throws(classOf[MemcacheServerException])
  def set(key: String, value: T, flags: Int, expiry: Int): Boolean = {
    setData(key, codec.encode(value), flags, expiry)
  }

  /**
   * Encode an item using the default codec and set it into the memcache pool.
   * The item's expiration will be "never".
   */
  @throws(classOf[MemcacheServerException])
  def set(key: String, value: T): Boolean = set(key, value, 0, 0)

  /**
   * Encode an item using the given codec and set it into the memcache pool.
   *
   * @param flogs arbitrary flags to be saved by the server (they have no intrinsic significance)
   * @param expiry absolute or relative time, in seconds or epoch itme, when this item should
   *   expire from the cache (0 = never)
   */
  @throws(classOf[MemcacheServerException])
  def set[A](key: String, value: A, flags: Int, expiry: Int, codec: MemcacheCodec[A]): Boolean = {
    setData(key, codec.encode(value), flags, expiry)
  }

  /**
   * Encode an item using the given codec and set it into the memcache pool.
   * The item's expiration will be "never".
   */
  @throws(classOf[MemcacheServerException])
  def set[A](key: String, value: A, codec: MemcacheCodec[A]): Boolean = set(key, value, 0, 0, codec)

  /**
   * Delete a given item from memcache
   */
  @throws(classOf[MemcacheServerException])
  def delete(key: String): Unit = {
    withNode(key) { (node, rkey) =>
      node.delete(rkey)
    }
  }

  /**
   * If nothing else is currently stored for this key, add an item to memcache as a byte array.
   *
   * @param flogs arbitrary flags to be saved by the server (they have no intrinsic significance)
   * @param expiry absolute or relative time, in seconds or epoch itme, when this item should
   *   expire from the cache (0 = never)
   * @return true if the item was added; false if something was already stored at this key
   */
  @throws(classOf[MemcacheServerException])
  def addData(key: String, value: Array[Byte], flags: Int, expiry: Int): Boolean = {
    withNode(key) { (node, rkey) =>
      node.add(rkey, value, flags, expiry)
    }
  }

  /**
   * If nothing else is currently stored for this key, add an item to memcache as a byte array.
   * The item's expiration will be "never".
   *
   * @return true if the item was added; false if something was already stored at this key
   */
  @throws(classOf[MemcacheServerException])
  def addData(key: String, value: Array[Byte]): Boolean = addData(key, value, 0, 0)

  /**
   * If nothing else is currently stored for this key, add an item to memcache using the default
   * codec.
   *
   * @param flogs arbitrary flags to be saved by the server (they have no intrinsic significance)
   * @param expiry absolute or relative time, in seconds or epoch itme, when this item should
   *   expire from the cache (0 = never)
   * @return true if the item was added; false if something was already stored at this key
   */
  @throws(classOf[MemcacheServerException])
  def add(key: String, value: T, flags: Int, expiry: Int): Boolean = {
    addData(key, codec.encode(value), flags, expiry)
  }

  /**
   * If nothing else is currently stored for this key, add an item to memcache using the default
   * codec.
   * The item's expiration will be "never".
   *
   * @return true if the item was added; false if something was already stored at this key
   */
  @throws(classOf[MemcacheServerException])
  def add(key: String, value: T): Boolean = add(key, value, 0, 0)

  /**
   * If nothing else is currently stored for this key, add an item to memcache using the given
   * codec.
   *
   * @param flogs arbitrary flags to be saved by the server (they have no intrinsic significance)
   * @param expiry absolute or relative time, in seconds or epoch itme, when this item should
   *   expire from the cache (0 = never)
   * @return true if the item was added; false if something was already stored at this key
   */
  @throws(classOf[MemcacheServerException])
  def add[A](key: String, value: A, flags: Int, expiry: Int, codec: MemcacheCodec[A]): Boolean = {
    addData(key, codec.encode(value), flags, expiry)
  }

  /**
   * If nothing else is currently stored for this key, add an item to memcache using the given
   * codec.
   * The item's expiration will be "never".
   *
   * @return true if the item was added; false if something was already stored at this key
   */
  @throws(classOf[MemcacheServerException])
  def add[A](key: String, value: A, codec: MemcacheCodec[A]): Boolean = add(key, value, 0, 0, codec)

  /**
   * If this key has a value, replace it with the given byte array.
   *
   * @param flogs arbitrary flags to be saved by the server (they have no intrinsic significance)
   * @param expiry absolute or relative time, in seconds or epoch itme, when this item should
   *   expire from the cache (0 = never)
   * @return true if the item was replaced; false if there was no data at this key
   */
  @throws(classOf[MemcacheServerException])
  def replaceData(key: String, value: Array[Byte], flags: Int, expiry: Int): Boolean = {
    withNode(key) { (node, rkey) =>
      node.replace(rkey, value, flags, expiry)
    }
  }

  /**
   * If this key has a value, replace it with the given byte array.
   * The item's expiration will be "never".
   *
   * @return true if the item was replaced; false if there was no data at this key
   */
  @throws(classOf[MemcacheServerException])
  def replaceData(key: String, value: Array[Byte]): Unit = replaceData(key, value, 0, 0)

  /**
   * If this key has a value, replace it with the given data encoded using the default codec.
   *
   * @param flogs arbitrary flags to be saved by the server (they have no intrinsic significance)
   * @param expiry absolute or relative time, in seconds or epoch itme, when this item should
   *   expire from the cache (0 = never)
   * @return true if the item was replaced; false if there was no data at this key
   */
  @throws(classOf[MemcacheServerException])
  def replace(key: String, value: T, flags: Int, expiry: Int): Unit = {
    replaceData(key, codec.encode(value), flags, expiry)
  }

  /**
   * If this key has a value, replace it with the given data encoded using the default codec.
   * The item's expiration will be "never".
   *
   * @return true if the item was replaced; false if there was no data at this key
   */
  @throws(classOf[MemcacheServerException])
  def replace(key: String, value: T): Unit = replace(key, value, 0, 0)

  /**
   * If this key has a value, replace it with the given data encoded using the given codec.
   *
   * @param flogs arbitrary flags to be saved by the server (they have no intrinsic significance)
   * @param expiry absolute or relative time, in seconds or epoch itme, when this item should
   *   expire from the cache (0 = never)
   * @return true if the item was replaced; false if there was no data at this key
   */
  @throws(classOf[MemcacheServerException])
  def replace[A](key: String, value: A, flags: Int, expiry: Int, codec: MemcacheCodec[A]): Unit = {
    replaceData(key, codec.encode(value), flags, expiry)
  }

  /**
   * If this key has a value, replace it with the given data encoded using the given codec.
   * The item's expiration will be "never".
   *
   * @return true if the item was replaced; false if there was no data at this key
   */
  @throws(classOf[MemcacheServerException])
  def replace[A](key: String, value: A, codec: MemcacheCodec[A]): Unit = replace(key, value, 0, 0, codec)

  /**
   * If this key has a value, append the given byte array to it.
   *
   * @return true if the item was appended; false if there was no data at this key
   */
  @throws(classOf[MemcacheServerException])
  def appendData(key: String, value: Array[Byte]): Boolean = {
    withNode(key) { (node, rkey) =>
      node.append(rkey, value, 0, 0)
    }
  }

  /**
   * If this key has a value, append the the given data to it, using the default codec.
   *
   * @return true if the item was appended; false if there was no data at this key
   */
  @throws(classOf[MemcacheServerException])
  def append(key: String, value: T): Boolean = {
    appendData(key, codec.encode(value))
  }

  /**
   * If this key has a value, prepend the given byte array to it.
   *
   * @return true if the item was prepended; false if there was no data at this key
   */
  @throws(classOf[MemcacheServerException])
  def prependData(key: String, value: Array[Byte]): Boolean = {
    withNode(key) { (node, rkey) =>
      node.prepend(rkey, value, 0, 0)
    }
  }

  /**
   * If this key has a value, prepend the the given data to it, using the default codec.
   *
   * @return true if the item was prepended; false if there was no data at this key
   */
  @throws(classOf[MemcacheServerException])
  def prepend(key: String, value: T): Boolean = {
    prependData(key, codec.encode(value))
  }

  /**
   * Increment a value stored at the given key.
   *
   * @return the value of the key after the increment or None of this key was not
   * perviously set
   */
  @throws(classOf[MemcacheServerException])
  def incr(key: String, value: Long): Option[Long] = {
    withNode(key) { (node, rkey) =>
      node.incr(rkey, value)
    }
  }

  /**
   * Decrement a value stored at the given key.
   *
   * @return the value of the key after the decrement or None of this key was not
   * perviously set
   */
  @throws(classOf[MemcacheServerException])
  def decr(key: String, value: Long): Option[Long] = {
    withNode(key) { (node, rkey) =>
      node.decr(rkey, value)
    }
  }

  /**
   * Return the server that would be used to fetch a key, using the current node locator.
   * The server name will be in the form "hostname:port:weight".
   *
   * @return the name of the server used to fetch the given key
   */
  def serverForKey(key: String): String = {
    withNode(key) { (node, rkey) =>
      "%s:%d:%d".format(node.hostname, node.port, node.weight)
    }
  }

  def nodeForKey(key: String): (MemcacheConnection, String) = {
    val realKey = namespace match {
      case None => key
      case Some(prefix) => prefix + key
    }
    if (realKey.length > MAX_KEY_SIZE) {
      throw new KeyTooLongException
    }
    (locator.findNode(realKey.getBytes("utf-8")), realKey)
  }

  private def withNode[T](key: String)(f: (MemcacheConnection, String) => T): T = {
    checkForUneject()
    val (node, realKey) = nodeForKey(key)
    withNode(node) { f(node, realKey) }
  }

  private def withNode[T](node: MemcacheConnection)(f: => T): T = {
    try {
      f
    } catch {
      case e: MemcacheClientError =>
        throw e
      case e: MemcacheServerException =>
        if (node.isEjected) {
          log.info("Ejecting from pool: %s", node)
          checkForEject()
        }
        throw e
    }
  }

  private def checkForUneject() {
    if (pool.shouldRebalance && pool.shouldRecheckEjectedConnections) {
      log.info("Retrying ejections...")
      locator.setPool(pool)
    }
  }

  private def checkForEject() {
    if (pool.shouldRebalance) {
      pool.scanForEjections()
      locator.setPool(pool)
    }
  }
}


/**
 * Factory methods for creating `MemcacheClient` objects.
 */
object MemcacheClient {
  /**
   * Create a new MemcacheClient from a server list and node locator, using
   * the UTF-8 codec.
   */
  def create(servers: Array[MemcacheConnection], locator: NodeLocator): MemcacheClient[String] = {
    create(servers, locator, MemcacheCodec.UTF8)
  }

  /**
   * Create a new MemcacheClient from a server list, node locator, and codec.
   * The codec will be the default mechanism for translating memcache items
   * to/from scala objects.
   */
  def create[T](servers: Array[MemcacheConnection], locator: NodeLocator,
                codec: MemcacheCodec[T]): MemcacheClient[T] = {
    val client = new MemcacheClient(locator, codec)
    val pool = new ServerPool
    pool.servers = servers
    client.setPool(pool)
    client
  }

  /**
   * Create a new MemcacheClient from a configgy block. The memcache cluster
   * distribution and hash function are specified by strings
   * "`distribution`" and "`hash`". An optional namespace
   * may be specified with "`namespace`". The server list must be
   * in a string list called "`servers`".
   */
  def create(attr: ConfigMap) = {
    val pool = ServerPool.fromConfig(attr)
    val locator = NodeLocator.byName(attr("distribution", "default")) match {
      case (hashName, factory) =>
        factory(KeyHasher.byName(attr("hash", hashName)))
    }
    val client = new MemcacheClient(locator, MemcacheCodec.UTF8)
    client.setPool(pool)
    client.namespace = attr.getString("namespace")
    client
  }

  /**
   * Create a new MemcacheClient from a server list, specifying the cluster's
   * distribution and hash function by name.
   */
  def create(servers: Array[String], distribution: String, hash: String) = {
    val pool = new ServerPool
    val connections = for (s <- servers) yield ServerPool.makeConnection(s, pool)
    pool.servers = connections

    val locator = NodeLocator.byName(distribution) match {
      case (hashName, factory) => factory(KeyHasher.byName(hash))
    }
    val client = new MemcacheClient(locator, MemcacheCodec.UTF8)
    client.setPool(pool)
    client
  }
}
