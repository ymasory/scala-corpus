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



package cc.factorie.generative
import cc.factorie._
import cc.factorie.la._
import scala.collection.mutable.{HashSet,ArrayBuffer}

// A collection of abstract Variables (and a generic Template) for generative models (directed Bayesian networks, 
// as opposed to undirected in which there is not a DAG-shaped generative storyline).

/** A variable whose value has been generated by a probability distribution parameterized by some parent variables.
    May or may not be mutable. */
trait GeneratedVar extends Variable {
  /** The list of random variables on which the generation of this variable's value depends. */
  def parents: Seq[Parameter]
  /** The list of random variables on which the generation of this variable's value depends,
      either directly or via a sequence of deterministic variables.  Changes to these variables
      cause the value of this.pr to change. */
  def extendedParents: Seq[Parameter] = {
    val result = new ArrayBuffer[Parameter]
    result ++= parents
    for (parent <- parents) parent match {
      case gv:GeneratedVar if (gv.isDeterministic) => result ++= gv.extendedParents
      case _ => {}
    }
    result
  }
  /** Parents, jumping over and selecting from MixtureComponents's parents if necessary. */
  def generativeParents: Seq[Parameter] = {
    val p = parents
    this match {
      case self:MixtureOutcome => parents.map(_ match { case mc:MixtureComponents[_] => mc(self.choice.intValue); case p:Parameter => p })
      case _ => parents
    }
  }
  /** Sometimes pointers to parents are kept in a ParameterRef variable; 
      if so, return them here so that we can track diffs to them; 
      if not, return Nil or null entries in sequence with ordering matching 'parents'. */
  def parentRefs: Seq[AbstractParameterRef] = Nil
  /** The probability of the current value given its parents. */
  def pr:Double // = prFrom(parents)
  def prFrom(parents:Seq[Parameter]): Double
  /** The log-probability of the current value given its parents. */
  def logpr:Double = math.log(pr)
  def logprFrom(parents:Seq[Parameter]): Double = math.log(prFrom(parents))
  def prWith(map:scala.collection.Map[Parameter,Parameter]): Double = prFrom(parents.map(p => map.getOrElse(p, p)))
  /** Returns true if the value of this parameter is a deterministic (non-stochastic) function of its parents. */
  def isDeterministic = false
}

/** A GeneratedVar that is mutable and whose value may be changed by sampling. */
trait GeneratedVariable extends GeneratedVar {
  /** Sample a new value for this variable given only its parents. */
  def sampleFromParents(implicit d:DiffList = null): this.type
  def sampleFromParentsWith(m:scala.collection.Map[Parameter,Parameter])(implicit d:DiffList = null): this.type = { sampleFrom(parents.map(p => m.getOrElse(p,p))); this }
  /** Sample a new value for this variable given only the specified parents, 
      ignoring its current registered parents. */
  // TODO I think this should be Seq[Parameter] below, not Seq[Variable]
  def sampleFrom(parents:Seq[Variable])(implicit d:DiffList): this.type
}


trait RealGenerating {
  def sampleDouble: Double
  def pr(x:Double): Double
  def logpr(x:Double): Double
}
trait DiscreteGenerating {
  def length: Int
  def sampleInt: Int
  def pr(index:Int): Double
  def logpr(index:Int): Double
}
trait ProportionGenerating {
  def sampleProportions: Proportions
  def pr(p:Proportions): Double
  def logpr(p:Proportions): Double
}





// Templates
class GeneratedVarTemplate extends TemplateWithStatistics3[GeneratedVar,MixtureChoiceVariable,Vars[Parameter]] {
  // TODO consider: override def _2: MixtureChoiceVariable = _1 match { case v:MixtureOutcome => v.choice; case _ => null }
  // And the same for _3.
  protected def factorOfGeneratedVar(v:GeneratedVar) = v match {
    // TODO Consider not bothering to fill in slots 2 and 3, just to save time and because it isn't necessary.  Yes!
    //case v:MixtureOutcome => Factor(v, v.choice, Vars.fromSeq(v.parents))
    //case _ => Factor(v, null, Vars.fromSeq(v.parents)) // TODO Consider just Factor(v, null, null) for efficiency
    case v:MixtureOutcome => Factor(v, null, null)
    case _ => Factor(v, null, null) // TODO Consider just Factor(v, null, null) for efficiency
  }
  def unroll1(v:GeneratedVar) = factorOfGeneratedVar(v)
  def unroll2(c:MixtureChoiceVariable) = c.outcomes.map(factorOfGeneratedVar(_)) //v => Factor(v, c, Vars.fromSeq(v.parents))
  def unroll3(vs:Vars[Parameter]) = throw new Error
  override def unroll3s(p:Parameter) = p match { 
    case m:MixtureComponents[_] => m.children.map(factorOfGeneratedVar(_))
    case p:Parameter => p.children.flatMap(_ match {
      case m:MixtureComponents[_] => m.childrenOf(p).map(factorOfGeneratedVar(_))
      case v:GeneratedVar => List(factorOfGeneratedVar(v))
    })
  }
  def score(s:Stat) = s._1.logpr // the log-probability of this GeneratedVar given its parents
}

/*
class GeneratedVarTemplate2 extends TemplateWithStatistics3[GeneratedVar,MixtureChoiceVariable,Vars[Parameter]] {
  protected def factorOfGeneratedVar(v:GeneratedVar) = v match {
    // TODO Consider not bothering to fill in slots 2 and 3, just to save time and because it isn't necessary
    case v:MixtureOutcome => Factor(v, v.choice, Vars.fromSeq(v.parents)) // Note that v.parents here is just the MixtureComponents[] object; constant over varying v.choice
    case _ => Factor(v, null, Vars.fromSeq(v.parents)) // TODO Consider just Factor(v, null, null) for efficiency
  }
  def unroll1(v:GeneratedVar) = factorOfGeneratedVar(v)
  def unroll2(c:MixtureChoiceVariable) = c.outcomes.map(factorOfGeneratedVar(_)) //v => Factor(v, c, Vars.fromSeq(v.parents))
  def unroll3(vs:Vars[Parameter]) = throw new Error("The Vars[Parameter] container itself should never change.  Individual Parameter changes are handled by unroll3s.")
  override def unroll3s(p:Parameter) = p match { 
    case m:MixtureComponents[_] => m.children.map(factorOfGeneratedVar(_))
    case p:Parameter => p.children.flatMap(_ match {
      case m:MixtureComponents[_] => m.childrenOf(p).map(factorOfGeneratedVar(_)) 
      // TODO Or alternatively just: factorOfGeneratedVar(m)? -akm
      // No, b/c then we could change a MixtureOutcome, x, and also change a Parameter in a MixtureComponents, and we would double-count p(x) -akm
      case v:GeneratedVar => List(factorOfGeneratedVar(v))
    })
  }
  def score(s:Stat) = s._1.logpr // the log-probability of this GeneratedVar given its parents
}
*/
