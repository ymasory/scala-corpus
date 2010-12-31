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
package kafka.examples;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import kafka.consumer.ConsumerConfig;
import kafka.consumer.ConsumerIterator;
import kafka.consumer.KafkaMessageStream;
import kafka.consumer.ZookeeperConsumerConnector;

public class Consumer extends Thread
{
  private final ZookeeperConsumerConnector consumer;
  private final String topic;
  
  public Consumer(String topic)
  {
    consumer = new ZookeeperConsumerConnector(createConsumerConfig());
    this.topic = topic;
  }

  private static ConsumerConfig createConsumerConfig()
  {
    Properties props = new Properties();
    props.put("zk.connect", KafkaProperties.zkConnect);
    props.put("groupid", KafkaProperties.groupId);
    props.put("zk.sessiontimeout.ms", "400");
    props.put("zk.synctime.ms", "200");
    props.put("autocommit.interval.ms", "1000");

    return new ConsumerConfig(props);

  }
 
  public void run() {
    Map<String, Integer> topicCountMap = new HashMap<String, Integer>();
    topicCountMap.put(topic, new Integer(1));
    Map<String, List<KafkaMessageStream>> consumerMap = consumer.createMessageStreams(topicCountMap);
    KafkaMessageStream stream =  consumerMap.get(topic).get(0);
    ConsumerIterator it = stream.iterator();
    while(it.hasNext())
      System.out.println(ExampleUtils.getMessage(it.next()));
  }
}
