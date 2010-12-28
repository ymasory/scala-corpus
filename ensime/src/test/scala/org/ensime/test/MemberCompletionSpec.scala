package org.ensime.test
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.ensime.test.util.Helpers._


class MemberCompletionSpec extends Spec with ShouldMatchers{

  describe("Type Member Completion") {

    it("should complete the member 'toString' of dude") {
      withPresCompiler{ cc =>
	val src = srcFile("Test1.scala", contents(
	    "import java.util.Vector",
	    "object Test1{",
	    "def main{",
	    "val dude = 1",
	    "dude",
	    "val horse = 2",
	    "}",
	    "}"
	  ))
	val mems = cc.askCompleteMemberAt(src.position(4,3), "toStr")
	mems.exists(s => s.name == "toString") should be(true)
      }
    }

    it("should complete the member 'main' of this") {
      withPresCompiler{ cc =>
	val src = srcFile("Test1.scala", contents(
	    "import java.util.Vector",
	    "object Test1{",
	    "def main{",
	    "val dude = 1",
	    "this",
	    "val horse = 2",
	    "}",
	    "}"
	  ))
	val mems = cc.askCompleteMemberAt(src.position(4,2), "mai")
	mems.exists(s => s.name == "main") should be(true)
      }
    }
  }

}
