import sbt._

class Project(info: ProjectInfo) extends DefaultProject(info) with Exec {
  
  val servlet = "javax.servlet" % "servlet-api" % "2.5" withSources //scalaz
  val scalacheck = "org.scala-tools.testing" %% "scalacheck" % "1.8"

  //project name
  override val artifactID = "scala-corpus"

  //managed dependencies from built-in repositories
  
  //turn down logging level to 'warn'
  log.setLevel(Level.Warn)

  //compiler options
  override def compileOptions = Deprecation :: Unchecked :: Nil //ExplainTypes
}
