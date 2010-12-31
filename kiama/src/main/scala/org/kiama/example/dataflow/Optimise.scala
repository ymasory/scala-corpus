/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010 Anthony M Sloane, Macquarie University.
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
package example.dataflow

import org.kiama.rewriting.Rewriter._

/**
 * Optimise a dataflow program.  Currently: a) eliminate assignments to
 * variables that are not live out of the assignment and b) remove empty
 * statements from sequences.
 */
object Optimise {

    import Dataflow._
    import DataflowAST._

    def run (t : Stm) : Stm =
        rewrite (rules) (t)

    lazy val rules = elimDeadAssign <* elimEmpties

    lazy val elimDeadAssign =
        alltd (rule {
            case s @ Assign (v, _) if (! (s->out contains v)) =>
                Empty ()
        })

    lazy val elimEmpties =
        bottomup (attempt (rule {
            case Empty () :: ss => ss
            case Block (Nil)    => Empty ()
        }))

}
