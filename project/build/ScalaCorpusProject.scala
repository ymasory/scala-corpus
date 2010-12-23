import sbt._

class Project(info: ProjectInfo) extends DefaultProject(info) with ScalazBoilerplate with Exec {

  //project name
  override val artifactID = "scala-corpus"

  //scalaz
  override def mainSourceRoots = super.mainSourceRoots +++ srcManagedScala##
  override def compileAction = super.compileAction dependsOn(generateTupleW)
  val servlet = "javax.servlet" % "servlet-api" % "2.5" withSources
  
  //turn down logging level to 'warn'
  log.setLevel(Level.Warn)

  //compiler options
  override def compileOptions = Deprecation :: Unchecked :: ExplainTypes :: Nil
}
