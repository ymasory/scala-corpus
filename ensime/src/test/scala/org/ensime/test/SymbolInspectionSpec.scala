package org.ensime.test
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import util.Helpers._

class SymbolInspectionSpec extends Spec with ShouldMatchers{
  
  describe("Symbol Info") {

    it("should see local variable declaration") {
      withPresCompiler{ cc =>
	val src = srcFile("Test1.scala", contents(
	    "object Test1{",
	    "def main{",
	    "val dude = 1",
	    "  ",
	    "}",
	    "}"
	  ))
	cc.askReloadAndTypeFiles(List(src))
	val info = cc.askSymbolInfoAt(src.position(2,5)).get
	info.name should equal("dude")
      }
    }
    
    it("should find local declaration position ") {
      withPresCompiler{ cc =>
    	val src = srcFile("Test2.scala", contents(
    	    "object Test2{",
    	    "def main{",
    	    "val dude = 1",
    	    "//space",
    	    "//space",
    	    "val horse = dude",
    	    "}",
    	    "}"
    	  ))
    	cc.askReloadAndTypeFiles(List(src))
    	val info = cc.askSymbolInfoAt(src.position(5,12)).get
    	info.declPos.line should equal(3)
      }
    }
  }

}
