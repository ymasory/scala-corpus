/* Copyright (c) 2008 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */


package com.google.xml.combinators

/**
 * Mix in this trait to get implicit conversions between functions of arity 2-7 and 
 * functions that take nested instances of '~' pairs.
 * 
 * This is particularly useful when pickling case classes. Complier generated functions for
 * apply and unapply are then implicitly converted to functions that take '~' instances.
 * 
 * Example:
 * <code>
 * case class Foo(x: Int, y: String)
 * 
 * wrap (elem("x", intVal) ~ elem("y", text)) Foo.apply Foo.unapply
 * 
 * @author Iulian Dragos
 */
trait TupleToPairFunctions {
  /** Convert a binary function to a function of a pair. */
  implicit def fun2ToPair[A, B, C](fun: (A, B) => C): (A ~ B) => C = { 
    case a ~ b => fun(a, b)
  }
    
  /** Convert a function of 3 arguments to one that takes a pair of a pair. */
  implicit def fun3ToPpairL[A, B, C, D](fun: (A, B, C) => D): (A ~ B ~ C) => D = { 
    case a ~ b ~ c =>  fun(a, b, c)
  }
    
  /** Convert a function of 4 arguments to one that takes a pair of a pair. */
  implicit def fun4ToPpairL[A, B, C, D, E]
      (fun: (A, B, C, D) => E): A ~ B ~ C ~ D => E = { 
    case a ~ b ~ c ~ d =>  fun(a, b, c, d)
  }

  /** Convert a function of 4 arguments to one that takes a pair of a pair. */
  implicit def fun5ToPpairL[A, B, C, D, E, F]
      (fun: (A, B, C, D, E) => F): (A ~ B ~ C ~ D ~ E) => F = { 
    case a ~ b ~ c ~ d ~ e =>  fun(a, b, c, d, e)
  }

  /** Convert a function of 4 arguments to one that takes a pair of a pair. */
  implicit def fun6ToPpairL[A, B, C, D, E, F, G]
      (fun: (A, B, C, D, E, F) => G): (A ~ B ~ C ~ D ~ E ~ F) => G = { 
    case a ~ b ~ c ~ d ~ e ~ f =>  fun(a, b, c, d, e, f)
  }

  /** Convert a function of 4 arguments to one that takes a pair of a pair. */
  implicit def fun7ToPpairL[A, B, C, D, E, F, G, H]
      (fun: (A, B, C, D, E, F, G) => H): (A ~ B ~ C ~ D ~ E ~ F ~ G) => H = { 
    case a ~ b ~ c ~ d ~ e ~ f ~ g =>  fun(a, b, c, d, e, f, g)
  }

  /** 
   * Convert a function of 3 arguments to one that takes a pair of a pair, 
   * right associative. 
   */
  implicit def fun3ToPpairR[A, B, C, D](f: (A, B, C) => D): (~[A, ~[B, C]]) => D = { 
    case a ~ (b ~ c) =>  f(a, b, c)
  } 
    
  implicit def funTuple2ToPair[A, B, C](f: A => (B, C)) = 
    { x: A => tuple2Pair(f(x)) }
  implicit def funTuple3ToPair[A, B, C, D](f: A => (B, C, D)) =
    { x: A => tuple3Pair(f(x)) }
  implicit def funTuple4ToPair[A, B, C, D, E](f: A => (B, C, D, E)) =
    { x: A => tuple4Pair(f(x)) }
  implicit def funTuple5ToPair[A, B, C, D, E, F](f: A => (B, C, D, E, F)) =
    { x: A => tuple5Pair(f(x)) }

  implicit def funTuple2ToPairUnapply[A, B, C](f: A => Option[(B, C)]) = 
    { x: A => tuple2Pair(f(x).get) }
  implicit def funTuple3ToPairUnapply[A, B, C, D](f: A => Option[(B, C, D)]) = 
    { x: A => tuple3Pair(f(x).get) }
  implicit def funTuple4ToPairUnapply[A, B, C, D, E](f: A => Option[(B, C, D, E)]) = 
    { x: A => tuple4Pair(f(x).get) }
  implicit def funTuple5ToPairUnapply[A, B, C, D, E, F](f: A => Option[(B, C, D, E, F)]) =
    { x: A => tuple5Pair(f(x).get) }
  
  def tuple2Pair[A, B](p: (A, B)) = new ~(p._1, p._2)
  def tuple3Pair[A, B, C](p: (A, B, C)) = new ~(p._1, p._2) ~ p._3
  def tuple4Pair[A, B, C, D](p: (A, B, C, D)) = new ~(p._1, p._2) ~ p._3 ~ p._4
  def tuple5Pair[A, B, C, D, E](p: (A, B, C, D, E)) = new ~(p._1, p._2) ~ p._3 ~ p._4 ~ p._5
  
}
