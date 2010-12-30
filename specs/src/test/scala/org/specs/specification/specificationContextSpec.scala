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
package org.specs.specification
import org.specs._

class specificationContextSpec extends SpecificationWithJUnit {
  "A specification context" can {
    "be used to specify the actions before the specification" in {
      specificationMustDo("beforeSpec")
    }    
    "be used to specify the actions after the specification" in {
      specificationMustDo("afterSpec")
    }    
    "be used to specify the actions before a sus" in {
      specificationMustDo("beforeSus")
    }    
    "be used to specify the actions after a sus" in {
      specificationMustDo("afterSus")
    }    
    "be used to specify the actions before an example" in {
      specificationMustDo("beforeExample")
    }    
    "be used to specify the actions after an example" in {
      specificationMustDo("afterExample")
    }   
    "be used to specify the actions around an example" in {
      specificationMustDo("around")
    }    
    "be used to specify the all actions" in {
      specificationMustDo("beforeSpec", 
                          "beforeSus", 
                          "beforeExample", 
                          "around", 
                          "afterExample", 
                          "afterSus", 
                          "afterSpec"
      )
    }
  }
  "A specification context" should {
    "not use around actions on a sus but only on examples and subexamples" in {
      val spec = new TestedSpecification("beforeSus", "beforeExample", "around", "afterExample", "afterSus") {
         "A system with a nested example" should {
           "have one example with a subexample" in {
             "this is a subexample" in { 1 must_== 1 }
           }
         }
      }
      specificationMustDo(spec, 
                          "beforeSus", 
                          "beforeExample", "around", "afterExample", 
                          "afterSus")
     }    
  }
  def specificationMustDo(s: String*): Any = {
    val spec = new SimpleSpecification(ContextParams(s:_*))
    specificationMustDo(spec)
  }
  def specificationMustDo(spec: TestedSpecification): Any = {
    specificationMustDo(spec: TestedSpecification, spec.params.values:_*)
  }
  def specificationMustDo(spec: TestedSpecification, expected: String*): Any = {
    noDetailedDiffs()
    spec.reportSpecs
    spec.out.toList aka spec.messages.mkString("\n") must_== expected.toList.map(_+"_ok")
  }
}
import org.specs.io.mock._

case class ContextParams(values: String*) {
  def contain(s: String) = values.contains(s)
}
class TestedSpecification(val params: ContextParams) extends Specification with MockOutput {
  def this(values: String*) = this(ContextParams(values:_*))
  val out = new scala.collection.mutable.ListBuffer[String]
  new SpecContext { 
    if (params.contain("beforeSpec")) beforeSpec(out.append("beforeSpec_ok")) 
    if (params.contain("beforeSus")) beforeSus(out.append("beforeSus_ok")) 
    if (params.contain("beforeExample")) before(out.append("beforeExample_ok")) 
    def output(a: =>Any) = { out.append("around_ok"); a }  
    if (params.contain("around")) aroundExpectations(output(_)) 
    if (params.contain("afterExample")) after(out.append("afterExample_ok"))
    if (params.contain("afterSus")) afterSus(out.append("afterSus_ok")) 
    if (params.contain("afterSpec")) afterSpec(out.append("afterSpec_ok")) 

  } 
}
class SimpleSpecification(params: ContextParams) extends TestedSpecification(params) {
  "this system" should { "have one example" in { 1 must_== 1 } }    
}
