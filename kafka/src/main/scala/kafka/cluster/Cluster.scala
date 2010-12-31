/*
 * Copyright 2010 LinkedIn
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package kafka.cluster

import kafka.utils._
import scala.collection._

/**
 * The set of active brokers in the cluster
 */
class Cluster {
  
  private val brokers = new mutable.HashMap[Int, Broker]
  
  def this(brokerList: Iterable[Broker]) {
    this()
	  for(broker <- brokerList)
      brokers.put(broker.id, broker)
  }

  def getBroker(id: Int) = brokers.get(id).get
  
  def add(broker: Broker) = brokers.put(broker.id, broker)
  
  def remove(id: Int) = brokers.remove(id)
  
  def size = brokers.size
  
  override def toString(): String = 
    "Cluster(" + brokers.values.mkString(", ") + ")"  
}
