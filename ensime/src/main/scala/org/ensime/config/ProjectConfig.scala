package org.ensime.config
import java.io.File
import org.ensime.util._
import org.ensime.util.FileUtils._
import org.ensime.util.RichFile._
import org.ensime.util.SExp._
import scala.actors._
import scala.actors.Actor._
import scala.collection.mutable
import scalariform.formatter.preferences._

object ProjectConfig {

  /**
  * Create a ProjectConfig instance from the given
  * SExp property list.
  */
  def fromSExp(config: SExpList) = {
    import ExternalConfigInterface._

    val m = config.toKeywordMap

    val rootDir: CanonFile = m.get(key(":root-dir")) match {
      case Some(StringAtom(str)) => new File(str)
      case _ => new File(".")
    }

    println("Using project root: " + rootDir)

    val sourceRoots = new mutable.HashSet[CanonFile]
    val runtimeDeps = new mutable.HashSet[CanonFile]
    val compileDeps = new mutable.HashSet[CanonFile]
    val classDirs = new mutable.HashSet[CanonFile]
    var target: Option[CanonFile] = None
    var projectName: Option[String] = None

    m.get(key(":use-sbt")) match {
      case Some(TruthAtom()) => {
	val depDirs = m.get(key(":sbt-subproject-dependencies")) match {
	  case Some(SExpList(deps)) => deps.map(_.toString)
	  case _ => List[String]()
	}
        println("Using sbt configuration..")
        val ext = getSbtConfig(rootDir, depDirs)
        projectName = ext.projectName
        sourceRoots ++= ext.sourceRoots
        runtimeDeps ++= ext.runtimeDepJars
        compileDeps ++= ext.compileDepJars
        target = ext.target
      }
      case _ =>
    }

    m.get(key(":use-maven")) match {
      case Some(TruthAtom()) => {
        println("Using maven configuration..")
        val ext = getMavenConfig(rootDir)
        projectName = ext.projectName
        sourceRoots ++= ext.sourceRoots
        runtimeDeps ++= ext.runtimeDepJars
        compileDeps ++= ext.compileDepJars
        target = ext.target
      }
      case _ =>
    }

    m.get(key(":use-ivy")) match {
      case Some(TruthAtom()) => {
        println("Using ivy configuration..")
        val rConf = m.get(key(":ivy-runtime-conf")).map(_.toString)
        val cConf = m.get(key(":ivy-compile-conf")).map(_.toString)
        val tConf = m.get(key(":ivy-test-conf")).map(_.toString)
        val file = m.get(key(":ivy-file")).map(s => new File(s.toString))
        val ext = getIvyConfig(rootDir, file, rConf, cConf, tConf)
        sourceRoots ++= ext.sourceRoots
        runtimeDeps ++= ext.runtimeDepJars
        compileDeps ++= ext.compileDepJars
        compileDeps ++= ext.testDepJars
        target = ext.target
      }
      case _ =>
    }

    m.get(key(":runtime-jars")) match {
      case Some(SExpList(items)) => {
        val jarsAndDirs = maybeFiles(items.map(_.toString), rootDir)
        val toInclude = expandRecursively(rootDir, jarsAndDirs, isValidJar _)
        println("Manually including " + toInclude.size + " run-time jars.")
        runtimeDeps ++= toInclude
      }
      case _ =>
    }

    m.get(key(":exclude-runtime-jars")) match {
      case Some(SExpList(items)) => {
        val jarsAndDirs = maybeFiles(items.map(_.toString), rootDir)
        val toExclude = expandRecursively(rootDir, jarsAndDirs, isValidJar _)
        println("Manually excluding " + toExclude.size + " run-time jars.")
        runtimeDeps --= toExclude
      }
      case _ =>
    }

    m.get(key(":compile-jars")) match {
      case Some(SExpList(items)) => {
        val jarsAndDirs = maybeFiles(items.map(_.toString), rootDir)
        val toInclude = expandRecursively(rootDir, jarsAndDirs, isValidJar _)
        println("Manually including " + toInclude.size + " compile-time jars.")
        compileDeps ++= toInclude
      }
      case _ =>
    }

    m.get(key(":exclude-compile-jars")) match {
      case Some(SExpList(items)) => {
        val jarsAndDirs = maybeFiles(items.map(_.toString), rootDir)
        val toExclude = expandRecursively(rootDir, jarsAndDirs, isValidJar _)
        println("Manually excluding " + toExclude.size + " compile-time jars.")
        compileDeps --= toExclude
      }
      case _ =>
    }

    m.get(key(":class-dirs")) match {
      case Some(SExpList(items)) => {
        val dirs = maybeDirs(items.map(_.toString), rootDir)
        println("Manually including " + dirs.size + " class directories.")
        classDirs ++= expand(rootDir, dirs, isValidClassDir _)
      }
      case _ =>
    }

    m.get(key(":sources")) match {
      case Some(SExpList(items)) => {
        val dirs = maybeDirs(items.map(_.toString), rootDir)
        println("Using source roots: " + dirs.mkString(", "))
        sourceRoots ++= dirs
      }
      case _ =>
    }

    m.get(key(":target")) match {
      case Some(StringAtom(targetDir)) => {
        target = target.orElse(maybeDir(targetDir, rootDir))
      }
      case _ =>
    }

    projectName = projectName.orElse(m.get(key(":project-name")).map(_.toString))

    val formatPrefs: Map[Symbol, Any] = m.get(key(":formatting-prefs")) match {
      case Some(list: SExpList) => {
        list.toKeywordMap.map {
          case (KeywordAtom(key), sexp: SExp) => (Symbol(key.substring(1)), sexp.toScala)
        }
      }
      case _ => Map[Symbol, Any]()
    }
    println("Using formatting preferences: " + formatPrefs)


    // Provide fix for 2.8.0 backwards compatibility
    val implicitNotFoundJar = new File("lib/implicitNotFound.jar")
    assert (implicitNotFoundJar.exists, { System.err.println(
	  "lib/implicitNotFound.jar not found! 2.8.0 compatibility may be broken.") })
    compileDeps += implicitNotFoundJar

    // Provide some reasonable defaults..

    target = verifyTargetDir(rootDir, target, new File(rootDir, "target/classes"))
    println("Using target directory: " + target.getOrElse("ERROR"))

    if (sourceRoots.isEmpty) {
      val f = new File("src")
      if (f.exists && f.isDirectory) {
        println("Using default source root, 'src'.")
        sourceRoots += f
      }
    }

    new ProjectConfig(
      projectName,
      rootDir, sourceRoots, runtimeDeps,
      compileDeps, classDirs, target,
      formatPrefs)

  }

  // If given target directory is not valid, use the default,
  // creating if necessary.
  def verifyTargetDir(rootDir: File, target: Option[File], defaultTarget: File): Option[CanonFile] = {
    val targetDir = target match {
      case Some(f: File) => {
        if (f.exists && f.isDirectory) { f } else { defaultTarget }
      }
      case None => defaultTarget
    }
    if (targetDir.exists) {
      Some(targetDir)
    } else {
      try {
        if (targetDir.mkdirs) Some(targetDir)
        else None
      } catch {
        case e => None
      }
    }
  }

  def nullConfig = new ProjectConfig(None, new File("."), List(),
    List(), List(), List(), None, Map())

  def getJavaHome(): Option[File] = {
    val javaHome: String = System.getProperty("java.home");
    if (javaHome == null) None
    else Some(new File(javaHome))
  }

  def javaBootJars(): Set[CanonFile] = {
    val javaHome = getJavaHome();
    javaHome match {
      case Some(javaHome) => {
        if (System.getProperty("os.name").startsWith("Mac")) {
          expandRecursively(
            new File("."),
            List(new File(javaHome, "../Classes")),
            isValidJar)
        } else {
          expandRecursively(
            new File("."),
            List(new File(javaHome, "lib")),
            isValidJar)
        }
      }
      case None => Set()
    }
  }
}

class ReplConfig(val classpath: String) {}

class DebugConfig(val classpath: String, val sourcepath: String) {}

class ProjectConfig(
  val name: Option[String],
  val root: CanonFile,
  val sourceRoots: Iterable[CanonFile],
  val runtimeDeps: Iterable[CanonFile],
  val compileDeps: Iterable[CanonFile],
  val classDirs: Iterable[CanonFile],
  val target: Option[CanonFile],
  formattingPrefsMap: Map[Symbol, Any]) {

  val formattingPrefs = formattingPrefsMap.
  foldLeft(FormattingPreferences()) { (fp, p) =>
    p match {
      case ('alignParameters, value: Boolean) =>
      fp.setPreference(AlignParameters, value)
      case ('compactStringConcatenation, value: Boolean) =>
      fp.setPreference(CompactStringConcatenation, value)
      case ('doubleIndentClassDeclaration, value: Boolean) =>
      fp.setPreference(DoubleIndentClassDeclaration, value)
      case ('formatXml, value: Boolean) =>
      fp.setPreference(FormatXml, value)
      case ('indentPackageBlocks, value: Boolean) =>
      fp.setPreference(IndentPackageBlocks, value)
      case ('indentSpaces, value: Int) =>
      fp.setPreference(IndentSpaces, value)
      case ('preserveSpaceBeforeArguments, value: Boolean) =>
      fp.setPreference(PreserveSpaceBeforeArguments, value)
      case ('rewriteArrowSymbols, value: Boolean) =>
      fp.setPreference(RewriteArrowSymbols, value)
      case ('spaceBeforeColon, value: Boolean) =>
      fp.setPreference(SpaceBeforeColon, value)
      case (name, _) => {
        System.err.println("Oops, unrecognized formatting option: " + name)
        fp
      }
    }
  }

  def compilerClasspathFilenames: Set[String] = {
    (compileDeps ++ classDirs).map(_.getPath).toSet
  }

  def sources: Set[CanonFile] = {
    expandRecursively(root, sourceRoots, isValidSourceFile _).toSet
  }

  def sourceFilenames: Set[String] = {
    sources.map(_.getPath).toSet
  }

  def compilerArgs = List(
    "-classpath", compilerClasspath,
    "-verbose")

  def builderArgs = List(
    "-classpath", compilerClasspath,
    "-verbose",
    "-d", target.getOrElse(new File(root, "classes")).getPath,
    sourceFilenames.mkString(" "))

  def compilerClasspath: String = {
    val files = compilerClasspathFilenames
    if (files.isEmpty) {
      "."
    } else {
      compilerClasspathFilenames.mkString(File.pathSeparator)
    }
  }

  def runtimeClasspath: String = {
    val deps = runtimeDeps ++ classDirs ++ target
    val paths = deps.map(_.getPath).toSet
    paths.mkString(File.pathSeparator)
  }

  def sourcepath = {
    sourceRoots.map(_.getPath).toSet.mkString(File.pathSeparator)
  }

  def replClasspath = runtimeClasspath

  def debugClasspath = runtimeClasspath

  def replConfig = new ReplConfig(replClasspath)

  def debugConfig = new DebugConfig(debugClasspath, sourcepath)

}

