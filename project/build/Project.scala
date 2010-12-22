import sbt._

class Project(info: ProjectInfo) extends DefaultProject(info) with Exec {
  
  //project name
  override val artifactID = "scala-corpus"

  //managed dependencies from built-in repositories
  
  //files to go in packaged jars
  val extraResources = "README.md" +++ "LICENSE"
  override val mainResources = super.mainResources +++ extraResources

  //turn down logging level to 'warn'
  log.setLevel(Level.Warn)

  //compiler options
  override def compileOptions = Deprecation :: Unchecked :: Nil //ExplainTypes
}
