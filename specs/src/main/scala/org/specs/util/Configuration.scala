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
package org.specs.util
import Classes._
import org.specs.io._

object Configuration extends Configuration {
  /** variable holding the current configuration which is the user configuration by default */
  var config = getUserConfiguration
}
trait Configuration extends ConfigurationFactory {
  /** this value controls if the errors stacktrace should be printed. */
  def stacktrace = true
  /** this value controls if ok examples should be printed. */
  def failedAndErrorsOnly = false
  /** this value controls if the statistics should be printed. */
  def statistics = true
  /** this value controls if the final statistics should be printed. */
  def finalStatisticsOnly = false
  /** this value controls if the ANSI color sequences should be used to colorize output */
  def colorize = false
  /** this value controls if examples without expectations should be marked as PENDING examples */
  def examplesWithoutExpectationsMustBePending = true
  /** this value controls if examples should be executed in a separate specification instance to avoid side effects */
  def oneSpecInstancePerExample = true
  /** this value controls if string differences should be displayed as highlighted */
  def smartDiffs = true
}
trait ConfigurationFactory extends FileSystem {
  /** @return the default configuration class */
  def getDefaultConfiguration: Configuration = new DefaultConfiguration 
  /** @return the user configuration class */
  def getUserConfiguration: Configuration = {
    getUserConfigurationFromPropertiesFile getOrElse( 
    getUserConfigurationFromClass getOrElse(
    getDefaultConfiguration))
    
  } 
  /** @return the configuration class named className and the default configuration otherwise. */
  def getConfiguration(name: String): Configuration = {
    getConfigurationFromPropertiesFile(name) getOrElse( 
    getConfigurationFromClass(name) getOrElse(
    getUserConfiguration))
  } 
  /** @return the configuration object from a class file */
  def getConfigurationFromClass(className: String): Option[Configuration] = {
    createObject[Configuration](className, false, false)
  } 
  /** @return the user configuration object from a properties file */
  def getConfigurationFromPropertiesFile(filePath: String): Option[Configuration] = {
    var configuration: Option[Configuration] = None
    try {
      val properties = new java.util.Properties()
      properties.load(inputStream(filePath))
      configuration = Some(new DefaultConfiguration {
        override def stacktrace = boolean(properties, "stacktrace", super.stacktrace)
        override def failedAndErrorsOnly = boolean(properties, "failedAndErrorsOnly", super.failedAndErrorsOnly)
        override def statistics = boolean(properties, "statistics", super.statistics)
        override def finalStatisticsOnly = boolean(properties, "finalStatisticsOnly", super.finalStatisticsOnly)
        override def colorize = boolean(properties, "colorize", super.colorize)
        override def examplesWithoutExpectationsMustBePending = boolean(properties, "examplesWithoutExpectationsMustBePending", super.examplesWithoutExpectationsMustBePending)
        override def oneSpecInstancePerExample = boolean(properties, "oneSpecInstancePerExample", super.oneSpecInstancePerExample)
        override def smartDiffs = boolean(properties, "smartDiffs", super.smartDiffs)
      })
    }
    catch {
      case _ => ()
    }
    configuration
  } 
  def boolean(properties: java.util.Properties, propName: String, defaultValue: Boolean) = {
    var prop = properties.get(propName)
    if (prop == null)
      defaultValue
    else {
      val propString = prop.toString.trim.toLowerCase
      if (propString.startsWith("y") || propString.startsWith("true"))
        true
      else if (propString.startsWith("n") || propString.startsWith("false"))
        false
      else
        defaultValue
    }
  } 
  /** @return the user configuration object from a class file */
  def getUserConfigurationFromClass: Option[Configuration] = {
    getConfigurationFromClass("configuration$")
  }
  /** @return the user configuration from a properties file */
  def getUserConfigurationFromPropertiesFile: Option[Configuration] = {
    getConfigurationFromPropertiesFile("configuration.properties")
  }
}
