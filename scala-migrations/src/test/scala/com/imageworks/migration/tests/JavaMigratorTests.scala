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
package com.imageworks.migration.tests

import org.junit.Assert._
import org.junit.{Before,
                  Test}

import com.imageworks.migration.{DuplicateMigrationDescriptionException,
                                 DuplicateMigrationVersionException,
                                 JavaDatabaseAdapter,
                                 JavaMigrator}

class JavaMigratorTests
{
  // Set the Derby system home to a test-databases directory so the
  // derby.log file and all databases will be placed in there.
  System.getProperties.setProperty("derby.system.home", "test-databases")

  // Load the Derby database driver.
  Class.forName("org.apache.derby.jdbc.EmbeddedDriver")

  private
  var java_migrator: JavaMigrator = _

  @Before
  def set_up(): Unit =
  {
    val db_name = System.currentTimeMillis.toString
    val url = "jdbc:derby:" + db_name + ";create=true"

    // The default schema for a Derby database is "APP".
    java_migrator = new JavaMigrator(
      url,
      JavaDatabaseAdapter.getDerbyDatabaseAdapter("APP"))
  }

  @Test(expected = classOf[DuplicateMigrationDescriptionException])
  def duplicate_descriptions_throw_exception: Unit =
  {
    java_migrator.installAllMigrations("com.imageworks.migration.tests.duplicate_descriptions",
                                       false)
  }

  @Test(expected = classOf[DuplicateMigrationVersionException])
  def duplicate_versions_throw_exception: Unit =
  {
    java_migrator.installAllMigrations("com.imageworks.migration.tests.duplicate_versions",
                                       false)
  }

  @Test
  def migrate_up_and_down: Unit =
  {
    // There should be no tables in the schema initially.
    assertEquals(0, java_migrator.getTableNames.size)

    // Migrate down the whole way.
    java_migrator.removeAllMigrations("com.imageworks.migration.tests.up_and_down",
                                      false)

    // The database should not be completely migrated.
    assertNotNull(java_migrator.whyNotMigrated("com.imageworks.migration.tests.up_and_down",
                                               false))

    // An empty array of Strings so that getTableNames.toArray returns
    // an Array[String] and not Array[AnyRef].
    val ea = new Array[String](0)

    // There should only be the schema migrations table now.
    assertEquals(1, java_migrator.getTableNames.size)
    assertFalse(java_migrator.getTableNames.toArray(ea).find(_.toLowerCase == "people").isDefined)

    // Apply all the migrations.
    java_migrator.installAllMigrations("com.imageworks.migration.tests.up_and_down",
                                       false)

    assertEquals(3, java_migrator.getTableNames.size)
    assertTrue(java_migrator.getTableNames.toArray(ea).find(_.toLowerCase == "people").isDefined)

    // The database should be completely migrated.
    assertNull(java_migrator.whyNotMigrated("com.imageworks.migration.tests.up_and_down",
                                            false))

    // Migrate down the whole way.
    java_migrator.removeAllMigrations("com.imageworks.migration.tests.up_and_down",
                                      false)

    // There should only be the schema migrations table now.
    assertEquals(1, java_migrator.getTableNames.size)
    assertFalse(java_migrator.getTableNames.toArray(ea).find(_.toLowerCase == "people").isDefined)
  }
}
