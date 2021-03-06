/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */



package cc.factorie
import cc.factorie.la._

/** For parameter estimation methods that use a gradient to update weight parameters. 
    @author Andrew McCallum */
trait WeightUpdates {
  type TemplatesToUpdate <: DotTemplate // TODO Was type TemplatesToUpdate <: DotTemplate, but this no longer works in Scala 2.8
  def templateClassToUpdate: Class[TemplatesToUpdate]
  /** The number of times 'updateWeights' has been called. */
  var updateCount : Int = 0
  /** Call this method to use the current gradient to change the weight parameters.  When you override it, you must call super.updateWeights. */
  def updateWeights : Unit = updateCount += 1
  /** Adds a gradient (calculated by the recipient) to the accumulator.  Abstract method to be provided elsewhere. */
  def addGradient(accumulator:TemplatesToUpdate=>Vector, rate:Double): Unit
}
