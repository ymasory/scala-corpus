import sbt._

class ScalaCorpusProject(info: ProjectInfo) extends DefaultProject(info)  with Exec {

  //project name
  override val artifactID = "scala-corpus"

  //turn down logging level to 'warn'
  log.setLevel(Level.Warn)

  //compiler options
  override def compileOptions = Deprecation :: Unchecked :: ExplainTypes :: Nil

  //projects
  lazy val scalaz = project("scalaz", "scalaz", new ScalazProject(_)) /* git 32757b4ed1f59dfbd121c9e483f06dada46ff792 */
  lazy val scalaquery = project("scala-query", "scala-query", new ScalaQueryProject(_)) /* git 0b9f9adfa15716ba4e5a659324000706ee1e42f7 */
  lazy val squeryl = project("squeryl", "squeryl", new SquerylProject(_)) /* git 0286ac918fd2e74f8f65e1cafe693e4d8f15f4ec */
  lazy val scalaswing = project("scala-swing", "scala-swing", new ScalaSwingProject(_)) /* git 5b45ba65a6afa15b6083bc4c0654d551a379e9a3 */
  lazy val ensime = project("ensime", "ensime", new EnsimeProject(_)) /* git b72d66ee73661735bc5a61e0d23874bbcaced76e */
  lazy val flashup = project("flashup", "flashup", new FlashupProject(_)) /* git 61aaf4159ef739068295efa03dd08ccbde76b600 */

  class FlashupProject(info: ProjectInfo) extends DefaultProject(info) {
    val scalatest = "org.scalatest" % "scalatest" % "1.2"
    val jsap = "com.martiansoftware" % "jsap" % "2.1"
  }

  class EnsimeProject(info: ProjectInfo) extends DefaultProject(info) {
    val scalaToolsSnapshots = "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots"
    val jbossRepo = "JBoss Maven 2 Repo" at "http://repository.jboss.org/maven2"
    val ant = "org.apache.ant" % "ant" % "1.8.1" % "compile;runtime;test"
    val ivy = "org.apache.ivy" % "ivy" % "2.1.0" % "compile;runtime;test"
    val maven = "org.apache.maven" % "maven-ant-tasks" % "2.1.0" % "compile;runtime;test"
    val scalatest = "org.scalatest" % "scalatest" % "1.2" % "test"
    val jdt = "org.eclipse.jdt" % "core" % "3.4.2.v_883_R34x" % "compile;runtime;test"
    val scalariform = "org.scalariform" % "scalariform_2.8.0" % "0.0.7"%"compile;runtime;test"
    val refactoring = "org.scala-refactoring" % "org.scala-refactoring.library" % "compile;runtime;test" from "http://scala-tools.org/repo-snapshots/org/scala-refactoring/org.scala-refactoring.library/0.3.0-SNAPSHOT/org.scala-refactoring.library-0.3.0-20101213.203503-7.jar"
    val asm = "asm" % "asm" % "3.2"
    val asmCommons = "asm" % "asm-commons" % "3.2"
  }

  class ScalaSwingProject(info: ProjectInfo) extends DefaultProject(info)

  class SquerylProject(info: ProjectInfo) extends DefaultProject(info) {
    val cglib = "cglib" % "cglib-nodep" % "2.2"
    val specs = "org.scala-tools.testing" % "specs_2.8.0" % "1.6.5" % "provided"  
  }

  class ScalazProject(info: ProjectInfo) extends DefaultProject(info) with ScalazBoilerplate {
    val servlets = "javax.servlet" % "servlet-api" % "2.5" withSources
    // val scalacheck = "org.scala-tools.testing" % "scalacheck_2.8.1" % "1.8" % "test"
    // val scalaToolsSnapshots = "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"
    // val specs = "org.scala-tools.testing" %% "specs" % "1.6.7-SNAPSHOT" % "test" withSources
    override def mainSourceRoots = super.mainSourceRoots +++ srcManagedScala##
    override def compileAction = super.compileAction dependsOn(generateTupleW)
  }

  class ScalaQueryProject(info: ProjectInfo) extends DefaultProject(info) {
    val junitInterface = "com.novocode" % "junit-interface" % "0.5" % "test"
  }
}
