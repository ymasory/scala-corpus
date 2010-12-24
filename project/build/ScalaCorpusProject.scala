import sbt._

class ScalaCorpusProject(info: ProjectInfo) extends DefaultProject(info)  with Exec {

  //project name
  override val artifactID = "scala-corpus"

  //turn down logging level to 'warn'
  log.setLevel(Level.Warn)

  //compiler options
  override def compileOptions = Deprecation :: Unchecked :: ExplainTypes :: Nil

  //projects
  lazy val scalaz = project("scalaz", "scalaz", new ScalazProject(_))

  class ScalazProject(info: ProjectInfo) extends DefaultProject(info) with ScalazBoilerplate {
    override def libraryDependencies = Set("javax.servlet" % "servlet-api" % "2.5" withSources)
    override def mainSourceRoots = super.mainSourceRoots +++ srcManagedScala##
    override def compileAction = super.compileAction dependsOn(generateTupleW)
  }
}
