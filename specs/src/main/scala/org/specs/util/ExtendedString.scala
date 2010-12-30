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

/** The ExtendedString object adds utility functions like 'uncapitalize' to Strings */
object ExtendedString {
  /** @return an ExtendedString */
  implicit def toExtendedString(s: String) = ExtendedString(s) 

  /** This class adds utility functions to Strings */
  case class ExtendedString(s: String) {
    /** @return the String s with its first character being uncapitalized: "HELLO".uncapitalize -> "hELLO" */
    def uncapitalize = s.head.toLower + s.drop(1)

    /** 
     * @param remove String to suppress from the original string
     * @return a String where every occurrence of remove has been suppressed 
     */
    def removeAll(remove: String) = s.replaceAll(toReplace(remove), "")
    private def toReplace(c: String) = c.map { letter => if ("()[]{}+-\\^$|?.*".contains(letter)) ("\\" + letter) else letter }.mkString("")
    
    /**
     * Remove everything from the first occurrence of a given substring.
     */
    def removeFrom(sub: String) = if (s.indexOf(sub) >= 0) s.substring(0, s.indexOf(sub)) else s
    
    /** 
     * @param pattern regexp pattern with groups (defined using parenthesis) specifying what to search in the string s
     * @return a list with every group found
     */
    def groups(pattern: String): List[String] = {
      if (pattern == null) return List[String]()
      val matcher = java.util.regex.Pattern.compile(pattern).matcher(s)
      val groupsFound = new scala.collection.mutable.ListBuffer[String]()
      while (matcher.find) { 
        try {
         groupsFound += matcher.group(1) 
        } catch { case _ => }
      }
      groupsFound.toList
    }
    def replaceGroups(pattern: String, function: String => Any): String = {
      groups(pattern).foldLeft(s) { (res: String, g: String) => s.replace(g, function(g).toString) }
    }
    
    /** 
     * This is a shortcut for groups("(" + group + ")")
     * @param group specification of the groups to find
     * @return a list with every group found
     */
    def findAll(group: String): List[String] = groups("(" + group + ")")
    
    /**
     * @return the uncamel-ed string: MyCamelString ->  My camel string
     */
    def uncamel = {
      def uncamelChars(chars : List[Char]): List[Char] = chars match {
        case c :: rest if (Character.isUpperCase(c)) => ' ' :: Character.toLowerCase(c) :: uncamelChars(rest)
        case c :: rest => c :: uncamelChars(rest)
        case Nil => Nil
      }
      if (s.isEmpty) ""
      else (s.charAt(0) :: uncamelChars(s.substring(1).toList)).mkString("")
    }
    
    /**
     * @return a list of Strings splitted so that they have a maximum size
     */
    def splitToSize(n: Int): List[String] = splitToSize(s, n, Nil)
    private def splitToSize(string: String, n: Int, result: List[String]): List[String] = {
      if (string.size <= n)
        (string :: result).reverse
      else
        // new Strings are necessary to avoid memory errors because substring is just a view on the underlying string
        splitToSize(new String(string.drop(n)), n, new String(string.take(n)) :: result)
    }
  }
}
