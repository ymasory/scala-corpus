/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2010 Anthony M Sloane, Macquarie University.
 *
 * Kiama is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 *
 * Kiama is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Kiama.  (See files COPYING and COPYING.LESSER.)  If not, see
 * <http://www.gnu.org/licenses/>.
 */

package org.kiama
package example.lambda2

/**
 * A simple lambda calculus abstract syntax.
 */
object AST {

    import org.kiama.attribution.Attributable
    import org.kiama.rewriting.Rewriter._

    /**
     * Identifiers are represented as strings.
     */
    type Idn = String

    /**
     * Expressions.
     */
    abstract class Exp extends Product with Attributable

    /**
     * Numeric expressions.
     */
    case class Num (n : Int) extends Exp {
        override def toString () = n.toString
    }

    /**
     * Variable expressions.
     */
    case class Var (i : Idn) extends Exp {
        override def toString () = i
    }

    /**
     * Lambda expressions binding name of type tipe within body.
     */
    case class Lam (i : Idn, t : Type, e : Exp) extends Exp {
        override def toString () =
            "(\\" + i + (if (t == null) "" else " : " + t) + " . " + e + ")"
    }

    /**
     * Application of l to r.
     */
    case class App (e1 : Exp, e2 : Exp) extends Exp {
        override def toString () = "(" + e1 + " " + e2 + ")"
    }

    /**
     * An application of a primitive binary operation.
     */
    case class Opn (o : Op, e1 : Exp, e2 : Exp) extends Exp {
        override def toString () = "(" + e1 + " " + o + " " + e2 + ")"
    }

    /**
     * Bind name of type tipe to the value of exp in body.
     */
    case class Let (i : Idn, t : Type, e1 : Exp, e2 : Exp) extends Exp {
        override def toString () = "(let " + i + " : " + t + " = " + e1 + " in " + e2 + ")"
    }

    /**
     * Parallel bindings in body.
     */
    case class Letp (bs : List[Bind], e : Exp) extends Exp {
        override def toString () = "(letp " + bs.mkString ("; ") + " in " + e + ")"
    }

    /**
     * A single binding from a set of parallel bindings (Letp).  No type
     * information because these bindings are only used inside the parallel
     * evaluation mechanisms.
     */
    case class Bind (i : Idn, e : Exp) {
        override def toString () = i + " = " + e
    }

    /**
     * Types.
     */
    abstract class Type extends Product with Attributable

    /**
     * Primitive integer type.
     */
    case object IntType extends Type {
        override def toString () = "Int"
    }

    /**
     * Function type from an argument type arg to a result type res.
     */
    case class FunType (t1 : Type, t2 : Type) extends Type {
        override def toString () = "" + t1 + " -> " + t2
    }

    /**
     * Primitive binary operators.
     */
    abstract class Op {
        /**
         * Evaluate the oeprator on the given integer operands.
         */
        def eval (l : Int, r : Int) : Int
    }

    /**
     * Primitive integer addition.
     */
    case object AddOp extends Op {
        def eval (l : Int, r : Int) = l + r
        override def toString () = "+"
    }

    /**
     * Primitive integer subtraction.
     */
    case object SubOp extends Op {
        def eval (l : Int, r : Int) = l - r
        override def toString () = "-"
    }

    // Congruences

    def Var (s1 : => Strategy) : Strategy =
        rulefs {
            case _ : Var =>
                congruence (s1)
        }

    def App (s1 : => Strategy, s2 : => Strategy) : Strategy =
        rulefs {
            case _ : App =>
                congruence (s1, s2)
        }

    def Lam (s1 : => Strategy, s2 : => Strategy, s3 : => Strategy) : Strategy =
        rulefs {
            case _ : Lam =>
                congruence (s1, s2, s3)
        }

    def Let (s1 : => Strategy, s2 : => Strategy, s3 : => Strategy, s4 : => Strategy) : Strategy =
        rulefs {
            case _ : Let =>
                congruence (s1, s2, s3, s4)
        }

    def Opn (s1 : => Strategy, s2 : => Strategy, s3 : => Strategy) : Strategy =
        rulefs {
            case _ : Opn =>
                congruence (s1, s2, s3)
        }

    def Letp (s1 : => Strategy, s2 : => Strategy) : Strategy =
        rulefs {
            case _ : Letp =>
                congruence (s1, s2)
        }

    def Bind (s1 : => Strategy, s2 : => Strategy) : Strategy =
        rulefs {
            case _ : Bind =>
                congruence (s1, s2)
        }

}
