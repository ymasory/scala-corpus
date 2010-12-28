package org.ensime.test
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import util.Helpers._

class PackageInspectionSpec extends Spec with ShouldMatchers{

  describe("Package Info") {

    it("should get a package description that includes the member 'Vector'") {
      withPresCompiler{ cc =>
	val src = srcFile("Test1.scala", contents(
	    "import java.util.Vector",
	    "object Test1{",
	    "def main{",
	    "val dude = 1",
	    "}",
	    "}"
	  ))
	cc.askReloadAndTypeFiles(List(src))
	val info = cc.askPackageByPath("java.util").get
	info.members.exists(m => m.name == "Vector") should be(true)
	info.members.exists(m => m.name == "List") should be(true)
      }
    }
    

  }
}
