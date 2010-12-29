package org.ensime.config
import java.io.File
import org.apache.ivy.{ core, util }
import org.apache.ivy.ant.IvyCacheTask
import org.apache.ivy.core.report.ArtifactDownloadReport
import org.apache.maven.artifact.ant._
import org.apache.tools.ant._
import org.ensime.util._
import org.ensime.util.FileUtils._
import scala.collection.JavaConversions._

case class ExternalConfig(
  val sourceRoots: Iterable[CanonFile],
  val runtimeDepJars: Iterable[CanonFile],
  val compileDepJars: Iterable[CanonFile],
  val testDepJars: Iterable[CanonFile],
  val target: Option[CanonFile]) {}

object ExternalConfigInterface {

  def getMavenConfig(baseDir: File): ExternalConfig = {
    val srcDirs = maybeDirs(List("src"), baseDir)
    val runtimeDeps = resolveMavenDeps(baseDir, "runtime")
    val compileDeps = resolveMavenDeps(baseDir, "compile")
    val testDeps = resolveMavenDeps(baseDir, "test")

    val f = new File(baseDir, "target/classes")
    val buildTarget = if (f.exists) { Some(toCanonFile(f)) } else { None }

    ExternalConfig(srcDirs, runtimeDeps, compileDeps, testDeps, buildTarget)
  }

  def resolveMavenDeps(baseDir: File, scopes: String): Iterable[CanonFile] = {
    println("Resolving Maven dependencies...")
    val project = new Project()
    project.addBuildListener(newConsoleLogger)
    project.setBaseDir(baseDir)
    project.init()
    val target = new Target()
    target.setName("ResolveDependencies")
    target.setProject(project)

    val pom = new Pom()
    pom.setFile(new File(baseDir, "pom.xml"))
    pom.setOwningTarget(target)
    pom.setProject(project)
    pom.setId("pom")
    target.addTask(pom)

    val task = new MavenDepsTask()
    task.setOwningTarget(target)
    task.setProject(project)
    task.addPom(pom)
    println("Using scopes: " + scopes)
    task.setScopes(scopes)
    target.addTask(task)

    project.addTarget("ResolveDependencies", target)

    try {
      project.executeTarget("ResolveDependencies")
    } catch {
      case e => {
        System.err.println("Failed to resolve Maven dependencies.")
        e.printStackTrace(System.err)
      }
    }

    task.deps.map(toCanonFile)
  }

  def getIvyConfig(baseDir: File,
    ivyFile: Option[File],
    runtimeConf: Option[String],
    compileConf: Option[String],
    testConf: Option[String]): ExternalConfig = {
    val srcDirs = maybeDirs(List("src"), baseDir)

    val resolve = { c: String => resolveIvyDeps(baseDir, ivyFile, c) }

    val defaultDeps = resolve("default")
    val runtimeDeps = runtimeConf.map(resolve(_)).getOrElse(defaultDeps)
    val compileDeps = compileConf.map(resolve(_)).getOrElse(defaultDeps)
    val testDeps = testConf.map(resolve(_)).getOrElse(defaultDeps)

    ExternalConfig(srcDirs, runtimeDeps, compileDeps, testDeps, None)
  }

  def resolveIvyDeps(baseDir: File, ivyFile: Option[File], conf: String): Iterable[CanonFile] = {
    println("Resolving Ivy dependencies...")
    val project = new Project()
    project.addBuildListener(newConsoleLogger)
    project.setBaseDir(baseDir)
    project.init()
    val target = new Target()
    target.setName("ResolveDependencies")
    target.setProject(project)

    val task = new IvyDepsTask()
    task.setOwningTarget(target)
    task.setProject(project)
    for (f <- ivyFile) {
      task.setFile(f)
      println("Using ivy file '" + f + "'.")
    }
    println("Using config '" + conf + "'.")
    target.addTask(task)

    project.addTarget("ResolveDependencies", target)

    try {
      project.executeTarget("ResolveDependencies")
    } catch {
      case e => {
        System.err.println("Failed to resolve Maven dependencies.")
        e.printStackTrace(System.err)
      }
    }

    task.deps.map(toCanonFile)
  }

  def getSbtConfig(baseDir: File): ExternalConfig = {
    val srcDirs = maybeDirs(List("src"), baseDir)
    val projectProps = new File(baseDir, "project/build.properties")
    val parentProjectProps = new File(baseDir, "../project/build.properties")

    val isMain = projectProps.exists
    val isSubProject = !(projectProps.exists) && parentProjectProps.exists

    if (isMain || isSubProject) {
      val propFile = if (isSubProject) { parentProjectProps } else { projectProps }
      println("Loading sbt build.properties from " + propFile + ".")
      val sbtConfig = SbtConfigParser(propFile)
      val v = sbtConfig.buildScalaVersion

      val runtimeDeps = resolveSbtDeps(baseDir, v, "runtime", isSubProject)
      val compileDeps = resolveSbtDeps(baseDir, v, "compile", isSubProject)
      val testDeps = resolveSbtDeps(baseDir, v, "test", isSubProject)

      val f = new File(baseDir, "target/scala_" + v + "/classes")
      val target = if (f.exists) { Some(toCanonFile(f)) } else { None }
      ExternalConfig(srcDirs, runtimeDeps, compileDeps, testDeps, target)
    } else {
      System.err.println("Could not locate build.properties file!")
      ExternalConfig(srcDirs, List(), List(), List(), None)
    }
  }

  def resolveSbtDeps(baseDir: File, scalaVersion: String, conf: String, isSubProject: Boolean): Iterable[CanonFile] = {
    println("Resolving sbt dependencies...")
    println("Using build config '" + conf + "'")
    val v = scalaVersion
    val unmanagedLibDir = "lib"
    val managedLibDir = "lib_managed/scala_" + v + "/" + conf
    val defaultManagedLibDir = "lib_managed/scala_" + v + "/default"
    val scalaLibDir = if (isSubProject) { "../project/boot/scala-" + v + "/lib" }
    else { "project/boot/scala-" + v + "/lib" }
    println("Using base directory " + baseDir)
    println("Searching for dependencies in " + unmanagedLibDir)
    println("Searching for dependencies in " + managedLibDir)
    println("Searching for dependencies in " + scalaLibDir)
    var jarRoots = maybeDirs(List(unmanagedLibDir, managedLibDir, defaultManagedLibDir, scalaLibDir), baseDir)
    val jars = expandRecursively(baseDir, jarRoots, isValidJar _)
    jars
  }

  private def newConsoleLogger = {
    val consoleLogger: DefaultLogger = new DefaultLogger()
    consoleLogger.setErrorPrintStream(System.err)
    consoleLogger.setOutputPrintStream(System.out)
    consoleLogger.setMessageOutputLevel(Project.MSG_INFO)
    consoleLogger
  }

}

class IvyDepsTask extends IvyCacheTask() {
  var deps: Iterable[File] = List()
  def doExecute() {
    prepareAndCheck()
    deps = getArtifactReports().map { a =>
      val art = a.asInstanceOf[ArtifactDownloadReport]
      art.getLocalFile()
    }
  }
}

