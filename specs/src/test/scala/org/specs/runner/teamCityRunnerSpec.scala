/**
 * Copyright (c) 2007-2010 Eric Torreborre <etorreborre@yahoo.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
 * documentation files (the "Software"), to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of
 * the Software. Neither the name of specs nor the names of its contributors may be used to endorse or promote
 * products derived from this software without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
package org.specs.runner

class teamCityRunnerSpec extends teamCityRunnerRules { "TeamCity Runner Specification" ->> <wiki>
 
h3. Introduction

The TeamCity runner is a Specification runner designed to create TeamCity formatted messages to the standard output when running specifications.
Those messages are then picked up by TeamCity and used to report the status of the current build

A specification for the TeamCity output messages can be found "here":http://www.jetbrains.net/confluence/display/TCD4/Build+Script+Interaction+with+TeamCity

h3. Notifying specifications and sus as Test suites

When executed, <ex>the TeamCity runner should notify the start of a specification</ex> with:

{"##teamcity[testSuiteStarted name='specification name']".as(message) >@}{messageMustBeCreated}

<ex>The end of the specification should also be reported</ex> as:

{"##teamcity[testSuiteFinished name='specification name']".as(message) >@}{messageMustBeCreated}

<ex>The systems under specification of a specification should be reported as test suite nested inside the specification suite</ex>:

{List("##teamcity[testSuiteStarted name='specification name']",
      "##teamcity[testSuiteStarted name='sus1 description']",
      "##teamcity[testSuiteFinished name='sus1 description']",
      "##teamcity[testSuiteFinished name='specification name']").as(messages).mkString("\n") >@}{messagesMustBeCreated}

<ex>The examples of system under specification should be reported as testStarted and testFinished inside the start and finished messages for the sus</ex>:

{List("##teamcity[testSuiteStarted name='specification name']", 
      "##teamcity[testSuiteStarted name='sus1 description']",
      "##teamcity[testStarted name='specification name.good example']",
      "##teamcity[testFinished name='specification name.good example']",
      "##teamcity[testSuiteFinished name='sus1 description']",
      "##teamcity[testSuiteFinished name='specification name']").as(messages).mkString("\n") >@}{messagesMustBeCreated}

+Note+: the name of the example is preceded by the specification name

<ex>A failed example must be reported with its failure message</ex>:
(the _details_ attribute is omitted)

{List("##teamcity[testStarted name='specification name.failed example']",
      "##teamcity[testFailed name='specification name.failed example' message='the value is true (teamCityRunnerRules.scala:38)' details='exception stacktrace']",
      "##teamcity[testFinished name='specification name.failed example']").as(messages).mkString("\n") >@}{messagesMustBeCreated}

<ex>An example with an exception must be reported with its error message</ex>:
(the _details_ attribute is omitted)

{List("##teamcity[testStarted name='specification name.exception example']",
      "##teamcity[testFailed name='specification name.exception example' message='error (teamCityRunnerRules.scala:39)' details='exception stacktrace']",
      "##teamcity[testFinished name='specification name.exception example']").as(messages).mkString("\n") >@}{messagesMustBeCreated}

<ex>An example with sub examples must be reported as one example with aggregated messages</ex>. For example, for the following subexamples:

{""""sub examples" >> {
  "good sub" in { true must beTrue }
  "bad sub1" in { true must beFalse }
  "bad sub2" in { false must beTrue }
}""".pre}

The messages should be (the _details_ attribute is omitted):

{List("##teamcity[testStarted name='specification name.sub examples']",
      "##teamcity[testFailed name='specification name.sub examples' message='bad sub1: the value is true (teamCityRunnerRules.scala:42); bad sub2: the value is false (teamCityRunnerRules.scala:43)' details='exception stacktrace']",
      "##teamcity[testFinished name='specification name.sub examples']").as(messages).mkString("\n")}{messagesMustBeCreated}

</wiki>
}



