/*
 * Copyright (c) 2009 Sony Pictures Imageworks Inc.
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the
 * distribution.  Neither the name of Sony Pictures Imageworks nor the
 * names of its contributors may be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package com.imageworks.migration

/**
 * This file contains all the case classes and case objects that act
 * as options to customize the SQL generated by the Migration methods.
 *
 * The design uses the Scala compiler to only allow valid options to
 * be passed to a Migration method.  Some case classes may be used by
 * multiple methods and since the base trait for the method's option
 * type may be sealed, they all need to be defined in a single source
 * file.
 */

/**
 * The base trait for all column options.  This is not a sealed class
 * so database specific column options can be defined.
 */
trait ColumnOption

/**
 * The base trait for all foreign key options.
 */
sealed trait ForeignKeyOption

/**
 * The base trait for all grant privilege types.
 */
sealed trait GrantPrivilegeType

/**
 * The base trait for all index options.
 */
sealed trait IndexOption

/**
 * The base trait for all check options.
 */
sealed trait CheckOption

/**
 * The base trait for all table options.
 */
sealed trait TableOption

/**
 * The base trait for all character set definitions.
 */
case class CharacterSet(name: CharacterSetName)
  extends ColumnOption

/**
 * A default value for a column.
 */
case class Default(value: String)
  extends ColumnOption

/**
 * Default companion object allowing Defaults to be constructed with
 * integer values.
 */
object Default {
  def apply(i: Int): Default =
  {
    Default(i.toString)
  }

  def apply(i: Long): Default =
  {
    Default(i.toString)
  }
}

/**
 * A limit on the size of a column type.
 */
case class Limit(expr: String)
  extends ColumnOption
{
  try {
    val length = Integer.parseInt(expr)
    if (length < 0) {
      val message = "The limit in " +
                    this +
                    " must be greater than or equal to one."
      throw new IllegalArgumentException(message)
    }
  }
  catch {
    case _: NumberFormatException =>
  }
}

/**
 * Limit companion object allowing Limits to be constructed with
 * integer values.
 */
object Limit {
  def apply(i: Int): Limit =
  {
    Limit(i.toString)
  }
}

/**
 * A name overriding the default index or foreign key constraint name
 * generated by Migration.
 */
case class Name(name: String)
  extends CheckOption
  with ForeignKeyOption
  with IndexOption
{
  if (name eq null) {
    throw new IllegalArgumentException("The name cannot be null.")
  }

  if (name.isEmpty) {
    throw new IllegalArgumentException("The name cannot be empty.")
  }
}

/**
 * Specify a check constraint on a column.
 */
case class Check(expr: String)
  extends ColumnOption

/**
 * Specify a named check constraint on a column.
 */
case class NamedCheck(name: String, expr: String)
  extends ColumnOption
// NamedCheck cannot inherit from Check, it causes a compiler error.
//   http://lampsvn.epfl.ch/trac/scala/ticket/425
// & http://lampsvn.epfl.ch/trac/scala/ticket/816

/**
 * Specify that the column's values must not be NULL.
 */
case object NotNull
  extends ColumnOption

/**
 * Specify that the column's values may be NULL.
 */
case object Nullable
  extends ColumnOption

/**
 * Specify the action that occurs when a value referenced in a foreign
 * key constraint is deleted.
 */
case class OnDelete(action: ForeignKeyConstraintAction)
  extends ForeignKeyOption

/**
 * Specify the action that occurs when a value referenced in a foreign
 * key constraint is updated.
 */
case class OnUpdate(action: ForeignKeyConstraintAction)
  extends ForeignKeyOption

/**
 * Specify the precision for a DECIMAL column.
 */
case class Precision(value: Int)
  extends ColumnOption
{
  if (value < 1) {
    val message = "The precision cannot be less than one."
    throw new IllegalArgumentException(message)
  }
}

/**
 * Specify that the column is a primary key.
 */
case object PrimaryKey
  extends ColumnOption

/**
 * Specify the scale for a DECIMAL column.
 */
case class Scale(value: Int)
  extends ColumnOption
{
  if (value < 0) {
    val message = "The scale cannot be less than zero."
    throw new IllegalArgumentException(message)
  }
}

/**
 * Specify that the index on the requires that all the values indexed
 * are unique.
 */
case object Unique
  extends ColumnOption
  with IndexOption

/**
 * Maps to GRANT ALL PRIVILEGES.
 */
case object AllPrivileges
  extends GrantPrivilegeType

/**
 * Maps to GRANT DELETE.
 */
case object DeletePrivilege
  extends GrantPrivilegeType

/**
 * Maps to GRANT INSERT.
 */
case object InsertPrivilege
  extends GrantPrivilegeType

/**
 * Maps to GRANT TRIGGER.
 */
case object TriggerPrivilege
  extends GrantPrivilegeType

/**
 * Scala 2.8 deprecates case classes extending other case classes.
 * Instead of implementing PrivilegeWithColumns as a case class to get
 * an extractor for the column names implement it as a non-case class
 * with an explicit extractor.
 */
object PrivilegeWithColumns
{
  /**
   * An extractor to return a sequence of column names.
   *
   * @param a any object
   * @return an optional sequence of column names
   */
  def unapply(a: Any): Option[Seq[String]] =
  {
    a match {
      case p: PrivilegeWithColumns =>
        Some(p.columns)
      case _ =>
        None
    }
  }
}

/**
 * A base class for all privileges that take a list of columns to affect.
 */
abstract class PrivilegeWithColumns
  extends GrantPrivilegeType
{
  val columns: Seq[String]
}

/**
 * Maps to GRANT REFERENCES.
 */
case class ReferencesPrivilege(override val columns: Seq[String])
  extends PrivilegeWithColumns

/**
 * Maps to GRANT SELECT.
 */
case class SelectPrivilege(override val columns: Seq[String])
  extends PrivilegeWithColumns

/**
 * Maps to GRANT UPDATE.
 */
case class UpdatePrivilege(override val columns: Seq[String])
  extends PrivilegeWithColumns

// These next three are here as case objects to allow
// a no-parameters form in user code
case object ReferencesPrivilege
  extends GrantPrivilegeType

case object SelectPrivilege
  extends GrantPrivilegeType

case object UpdatePrivilege
  extends GrantPrivilegeType

/**
 * This class is defined to prevent ant from recompiling this source
 * file.
 */
private
class Options
