import sbt._

class ScalaCorpusProject(info: ProjectInfo) extends DefaultProject(info)  with Exec {

  //project name
  override val artifactID = "scala-corpus"

  //turn down logging level to 'warn'
  log.setLevel(Level.Warn)

  //compiler options
  override def compileOptions = Deprecation :: Unchecked :: ExplainTypes :: Nil

  // override def libraryDependencies = Set()

  //projects
  lazy val scalaz = project("scalaz", "scalaz", new ScalazProject(_)) //git 32757b4ed1f59dfbd121c9e483f06dada46ff792
  lazy val scalaquery = project("scala-query", "scala-query", new ScalaQueryProject(_)) //git 0b9f9adfa15716ba4e5a659324000706ee1e42f7
  lazy val squeryl = project("squeryl", "squeryl", new SquerylProject(_)) //git 0286ac918fd2e74f8f65e1cafe693e4d8f15f4ec

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
