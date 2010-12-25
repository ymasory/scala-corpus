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
  lazy val scalaz = project("scalaz", "scalaz", new ScalazProject(_))
  lazy val scalaquery = project("scala-query", "scala-query", new ScalaQueryProject(_))

  class ScalazProject(info: ProjectInfo) extends DefaultProject(info) with ScalazBoilerplate {
    val servlets = "javax.servlet" % "servlet-api" % "2.5" withSources
    val scalacheck = "org.scala-tools.testing" % "scalacheck_2.8.1" % "1.8" % "test"
    val specs = "org.scala-tools.testing" % "specs" % "1.6.2.2" % "test"
    override def mainSourceRoots = super.mainSourceRoots +++ srcManagedScala##
    override def compileAction = super.compileAction dependsOn(generateTupleW)
  }

  class ScalaQueryProject(info: ProjectInfo) extends DefaultProject(info) {
    val junitInterface = "com.novocode" % "junit-interface" % "0.5" % "test"
  }
}
