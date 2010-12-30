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
import org.specs._
import org.specs.specification._
import org.specs.runner._
import org.specs.matcher._

class editDistanceSpec extends SpecificationWithJUnit with EditDistance with DataTables {
  "The edit distance" should {
    "return 0 if there's no insertions" in {
      editDistance("kitte", "kitte") must_== 0
    }
    "work on insertions" in {
      editDistance("kitte", "kittei") must_== 1
    }
    "work on suppressions" in {
     editDistance("kitten", "kit") must_== 3
    }
    "work on substitutions" in {
     editDistance("kitten", "kitsin") must_== 2
    }
  }
  "The show distance" should {
    "work on insertions" in {
      showDistance("kitte", "kittei") must_== ("kitte[]", "kitte[i]")
      showDistance("kitten", "kittein") must_== ("kitte[]n", "kitte[i]n")
    }
    "work on suppressions" in {
      showDistance("kitten", "kit") must_== ("kit[ten]", "kit[]")
    }
    "work on suppressions - 2" in {
      showDistance("kit", "kitten") must_== ("kit[]", "kit[ten]")
    }
    "work on substitutions" in {
      showDistance("kitten", "kitsin") must_== ("kit[te]n", "kit[si]n")
    }
    "not show any difference for the same string" in {
      showDistance("kitte", "kitte") must_== ("kitte", "kitte")
    }
    "show the differences with another separator" in {
      showDistance("kitten", "kitsin", "()") must_== ("kit(te)n", "kit(si)n")
    }
    "show the differences with another separator like <<>>" in {
      showDistance("kitten", "kitsin", "<<>>") must_== ("kit<<te>>n", "kit<<si>>n")
    }
    "show the differences with another separator like <<+" in {
      showDistance("kitten", "kitsin", "<<+") must_== ("kit<<te+n", "kit<<si+n")
    }
    "work on 0-sized strings" in {
       "a"	| "b" 		| "result" 			|>
       "" 	! ""	   	! ("", "")    		|
       "" 	! "a"	   	! ("[]", "[a]")     |
       "a" 	! ""	   	! ("[a]", "[]")    	|
       "" 	! "ab"	   	! ("[]", "[ab]")	|
       "ab" ! ""   		! ("[ab]", "[]") 	|
       { (a: String, b: String, result: (String, String)) =>
         showDistance(a, b) must_== result
       }
    }
    "work on 1-sized strings" in {
       "a"	| "b" 		| "result" 			|>
       "a" 	! "a"	   	! ("a", "a")    	|
       "a" 	! "b"	   	! ("[a]", "[b]")	|
       "a" 	! "bc"	   	! ("[a]", "[bc]")  	|
       "a" 	! "ab"	   	! ("a[]", "a[b]")	|
       { (a: String, b: String, result: (String, String)) =>
         showDistance(a, b) must_== result
       }
    }
  }
  "The diff shortener" should {
    import DiffShortener._
    "not shorten a regular string" in {
      shorten("abcd") must_== "abcd"
    }
    "shorten a diff before if too many letters" in {
      shorten("abcdefghijkl(mn)opqr") must_== "...hijkl(mn)opqr"
    }
    "shorten a diff even at the end" in {
      shorten("abcdefghijkl(mn)") must_== "...hijkl(mn)"
    }
    "shorten a diff even at the beginning" in {
      shorten("(mn)abcdefghijkl") must_== "(mn)abcde..."
    }
    "shorten right and left" in {
      shorten("abcdefghijkl(mn)opqrstuv") must_== "...hijkl(mn)opqrs..."
    }
    "shorten in the center" in {
      shorten("hijkl(zz)abcdefghijklmno(xx)abcde") must_== "hijkl(zz)ab...no(xx)abcde"
    }
    "shorten in the center with an empty diff" in {
      shorten("hijkl()xxabcdefghijklmno()xxabcde") must_== "hijkl()xx...no()xxabc..."
    }
    "shorten in the center with an empty diff" in {
      shorten("abcdef()ghijkl") must_== "...bcdef()ghijk..."
    }
    "shorten left, center and right" in {
      shorten("abcdefg(zz)abcdefghijklmno(xx)abcdefg") must_== "...cdefg(zz)ab...no(xx)abcde..."
    }
  }
  "The edit distance algorithm" should {
    "dont use too much memory on a big string comparison when working with file lines" in {
      val factor = 1000
      editDistance("kitten\n" * factor, "kitsin\n" * factor) must be > 0
      showDistance("kitten\n" * factor, "kitsin\n" * factor)._1.size must be > 0
    }
    "dont use too much memory on a big string comparison on any type of string" in {
      val factor = 1000
      editDistance("kitten" * factor, "kitsin" * factor) must be > 0
      showDistance("kitten" * factor, "kitsin" * factor)._1.size must be > 0
    }
  }
}
