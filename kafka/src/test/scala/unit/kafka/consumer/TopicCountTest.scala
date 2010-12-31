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

package kafka.consumer

import junit.framework.TestCase
import junit.framework.Assert._

class TopicCountTest extends TestCase {

  def testBasic() {
    val consumer = "conusmer1"
    val json = """{ "topic1" : 2, "topic2" : 3 }"""
    val topicCount = TopicCount.constructTopicCount(consumer, json)
    val topicCountMap = Map(
      "topic1" -> 2,
      "topic2" -> 3
      )
    val expectedTopicCount = new TopicCount(consumer, topicCountMap)
    assertTrue(expectedTopicCount == topicCount)

    val topicCount2 = TopicCount.constructTopicCount(consumer, expectedTopicCount.toJsonString)
    assertTrue(expectedTopicCount == topicCount2)
  }
}
