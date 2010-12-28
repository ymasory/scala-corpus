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
import scala.collection.mutable.ListBuffer
import java.util.Properties

case class ExternalConfig(
  val projectName: Option[String],
  val sourceRoots: Iterable[CanonFile],
  val runtimeDepJars: Iterable[CanonFile],
  val compileDepJars: Iterable[CanonFile],
  val testDepJars: Iterable[CanonFile],
  val target: Option[CanonFile]) {}

object ExternalConfigInterface {

  def getMavenConfig(baseDir: File): ExternalConfig = {
    val srcPaths = maybeDirs(List(
      "src/main/scala",
      "src/main/java",
      "src/test/scala",
      "src/test/java"), baseDir)
    val runtimeDeps = resolveMavenDeps(baseDir, "runtime")
    val compileDeps = resolveMavenDeps(baseDir, "compile")
    val testDeps = resolveMavenDeps(baseDir, "test")

    val f = new File(baseDir, "target/classes")
    val buildTarget = if (f.exists) { Some(toCanonFile(f)) } else { None }

    ExternalConfig(None, srcPaths, runtimeDeps, compileDeps, testDeps, buildTarget)
  }

  def resolveMavenDeps(baseDir: File, conf: String): Iterable[CanonFile] = {

    // Recreate the default maven classpaths.
    val scopes = conf match {
      case "compile" => "compile,provided,system,test"
      case "runtime" => "compile,provided,system,runtime"
      case "test" => "compile,provided,system,runtime,test"
    }

    println("\n\nResolving Maven dependencies for ensime config: " + conf)
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
    println("Resolving with scopes: " + scopes)
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

    val srcPaths = maybeDirs(List(
      "src/main/scala",
      "src/main/java",
      "src/test/scala",
      "src/test/java"), baseDir)

    val resolve = { c: String => resolveIvyDeps(baseDir, ivyFile, c) }

    val defaultDeps = resolve("default")
    val runtimeDeps = runtimeConf.map(resolve(_)).getOrElse(defaultDeps)
    val compileDeps = compileConf.map(resolve(_)).getOrElse(defaultDeps)
    val testDeps = testConf.map(resolve(_)).getOrElse(defaultDeps)

    ExternalConfig(None, srcPaths, runtimeDeps, compileDeps, testDeps, None)
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

  def getSbtConfig(baseDir: File, deps: Iterable[String]): ExternalConfig = {

    val projectProps = new File(baseDir, "project/build.properties")
    val parentProjectProps = new File(baseDir, "../project/build.properties")
    val isSubProject = !(projectProps.exists) && parentProjectProps.exists
    val propFile = if (isSubProject) { parentProjectProps } else { projectProps }
    println("Loading sbt build.properties from " + propFile + ".")
    val props = JavaProperties.load(propFile)

    val v = props.get("build.scala.versions").map(_.toString).getOrElse("2.8.0")
    val projName = props.get("project.name").map(_.toString)
    println("Scala Build version is " + v)

    val f = new File(baseDir, "target/scala_" + v + "/classes")
    val target = if (f.exists) { Some(toCanonFile(f)) } else { None }

    val compileDeps = ListBuffer[CanonFile]()
    val runtimeDeps = ListBuffer[CanonFile]()
    val testDeps = ListBuffer[CanonFile]()
    val srcPaths = ListBuffer[CanonFile]()

    val scalaLibDir = if (isSubProject) {
      "../project/boot/scala-" + v + "/lib"
    } else {
      "project/boot/scala-" + v + "/lib"
    }

    println("Searching for scala libs in " + scalaLibDir)
    var jarRoots = maybeDirs(List(scalaLibDir), baseDir)
    val scalaJars = expandRecursively(baseDir, jarRoots, isValidJar _)
    compileDeps ++= scalaJars
    runtimeDeps ++= scalaJars
    testDeps ++= scalaJars

    println("Adding this project's dependencies..")
    val info = getSbtProjectInfo(baseDir, v)
    compileDeps ++= info.compileDeps
    runtimeDeps ++= info.runtimeDeps
    testDeps ++= info.testDeps
    srcPaths ++= info.srcPaths

    if (isSubProject) {
      println("Adding subproject dependencies..")
      for (proj <- deps) {
        val dir = new File(baseDir, "../" + proj)
        val info = getSbtProjectInfo(dir, v)
        compileDeps ++= info.compileDeps
        runtimeDeps ++= info.runtimeDeps
        testDeps ++= info.testDeps
        srcPaths ++= info.srcPaths
      }
    }

    ExternalConfig(projName, srcPaths, runtimeDeps, compileDeps, testDeps, target)
  }

  case class SbtProjectInfo(
    srcPaths: Iterable[CanonFile],
    runtimeDeps: Iterable[CanonFile],
    compileDeps: Iterable[CanonFile],
    testDeps: Iterable[CanonFile])

  def getSbtProjectInfo(baseDir: File, buildScalaVersion: String): SbtProjectInfo = {
    val srcPaths = maybeDirs(List(
      "src/main/scala",
      "src/main/java",
      "src/test/scala",
      "src/test/java"), baseDir)
    val compileDeps = resolveSbtDeps(baseDir, buildScalaVersion, "compile")
    val runtimeDeps = resolveSbtDeps(baseDir, buildScalaVersion, "runtime")
    val testDeps = resolveSbtDeps(baseDir, buildScalaVersion, "test")
    SbtProjectInfo(srcPaths, runtimeDeps, compileDeps, testDeps)
  }

  def resolveSbtDeps(baseDir: File, scalaVersion: String,
    conf: String): Iterable[CanonFile] = {

    println("Resolving sbt dependencies at directory: " + baseDir)
    println("Using build config '" + conf + "'")

    // Recreate the default sbt classpaths.
    // Except we include 'test' in the compile configuration, since ENSIME needs to analyze test sources..
    val confs = conf match {
      case "compile" => List("compile", "default", "provided", "optional", "test")
      case "runtime" => List("compile", "default", "provided", "optional", "runtime")
      case "test" => List("compile", "default", "provided", "optional", "runtime", "test")
    }

    val v = scalaVersion
    val unmanagedLibDir = "lib"
    val managedDirs = confs.map { c => "lib_managed/scala_" + v + "/" + c }
    val jarDirs = unmanagedLibDir :: managedDirs
    println("Searching for dependencies in " + jarDirs)
    var jarRoots = maybeDirs(jarDirs, baseDir)
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

