package org.ensime.debug
import org.ensime.util._
import org.ensime.util.RichFile._
import org.ensime.util.FileUtils._
import org.ensime.config.ProjectConfig
import scala.collection.mutable.{ HashMap, ArrayBuffer }
import java.io._
import org.objectweb.asm._
import org.objectweb.asm.commons.EmptyVisitor
import scala.math._

case class DebugSourceLinePairs(pairs: Iterable[(String, Int)])

class ProjectDebugInfo(projectConfig: ProjectConfig) {

  private val target: File = projectConfig.target.getOrElse(new File("."))


  /**
   * For each (classname,line) pair, return the corresponding
   * (sourcefile,line) pair.
   */
  def debugClassLocsToSourceLocs(pairs: Iterable[(String, Int)]): DebugSourceLinePairs = {
    DebugSourceLinePairs(pairs.map { ea =>
      findSourceForClass(ea._1) match {
        case Some(s) => (s, ea._2)
        case None => ("", ea._2)
      }
    })
  }

  /**
   *
   * Returns the unit whose bytecodes correspond to the given
   * source location.
   *
   * @param  source  The source filename, without path information.
   * @param  line    The source line
   * @param  packPrefix  A possibly incomplete prefix of the desired unit's package
   * @return         The desired unit
   */
  def findUnit(source: String, line: Int, packPrefix: String): Option[DebugUnit] = {
    val units = sourceNameToUnits(source)
    units.find { u =>
      u.startLine <= line &&
        u.endLine >= line &&
        u.packageName.startsWith(packPrefix)
    }
  }

  def findSourceForClass(className: String): Option[String] = {
    val paths = classNameToSourcePath(className)

    // TODO: Loss of precision here!
    paths.headOption
  }

  private val sourceNameToSourcePath = new HashMap[String, ArrayBuffer[String]] {
    override def default(s: String) = new ArrayBuffer[String]
  }
  projectConfig.sources.foreach { f =>
    val paths = sourceNameToSourcePath(f.getName)
    paths += f.getAbsolutePath
    sourceNameToSourcePath(f.getName) = paths
  }

  private val classNameToSourcePath = new HashMap[String, ArrayBuffer[String]] {
    override def default(s: String) = new ArrayBuffer[String]
  }

  private val sourceNameToUnits = new HashMap[String, ArrayBuffer[DebugUnit]] {
    override def default(s: String) = new ArrayBuffer[DebugUnit]
  }

  if (target.exists && target.isDirectory) {

    val classFiles = target.andTree.toList.filter { f =>
      !f.isHidden && f.getName.endsWith(".class")
    }

    classFiles.foreach { f =>
      val fs = new FileInputStream(f)
      try {
        val reader = new ClassReader(fs)
        reader.accept(new EmptyVisitor() {
          var qualName: String = null
          var packageName: String = null
          var sourceName: String = null
          var startLine = Int.MaxValue
          var endLine = Int.MinValue

          override def visit(version: Int, access: Int,
            name: String, signature: String, superName: String,
            interfaces: Array[String]) {

            if (name != null) {
              qualName = name.replace("/", ".")
              packageName = qualName.split(".").lastOption.getOrElse("")
              println("")
              println("--------")
              println(qualName)
            }
          }

          override def visitAttribute(att: Attribute) {
            println("Attribute:" + att.`type`)
          }

          override def visitAnnotation(desc: String,
            visibleAtRuntime: Boolean): AnnotationVisitor = {
            println("Annotation: " + desc)
            new EmptyVisitor() {
              override def visit(name: String, value: Object) {
                println(name)
              }
              override def visitArray(name: String): AnnotationVisitor = {
                println("Array:" + name)
                null
              }
              override def visitEnum(name: String, desc: String, value: String) {
                println("Enum:" + name)
              }
            }
          }

          override def visitField(i: Int, s1: String,
            s2: String, s3: String, a: Any): FieldVisitor = null

          override def visitMethod(access: Int, name: String,
            desc: String, signature: String,
            exceptions: Array[String]): MethodVisitor = {
            println("Method: " + signature + " " + name)
            new EmptyVisitor() {

              override def visitLineNumber(line: Int, start: Label) {
                println("  line: " + line + ", label=" + start)
                startLine = min(startLine, line)
                endLine = max(endLine, line)
              }

              override def visitAttribute(att: Attribute) {}
              override def visitAnnotation(desc: String,
                visibleAtRuntime: Boolean): AnnotationVisitor = null
              override def visitAnnotationDefault(): AnnotationVisitor = null
              override def visitParameterAnnotation(parameter: Int,
                desc: String, visible: Boolean): AnnotationVisitor = null
            }
          }

          override def visitSource(source: String, debug: String) {
            sourceName = source
          }

          override def visitEnd() {
            val possibleSourcePaths = sourceNameToSourcePath(sourceName)
            possibleSourcePaths.foreach { p =>
              val paths = classNameToSourcePath(qualName)
              paths += p
              classNameToSourcePath(qualName) = paths
            }

            // Notice that a single name may resolve to many units.
            // This is either due to many classes/objects declared in one file,
            // or the fact that the mapping from source name to source path is 
            // one to many.
            val units = sourceNameToUnits(sourceName)
            val newU = new DebugUnit(startLine, endLine, f,
              sourceName, packageName, qualName)
            units += newU

            // Sort in descending order of startLine, so first unit found will also be 
            // the most deeply nested.
            val sortedUnits = units.sortWith { (a, b) => a.startLine > b.startLine }

            sourceNameToUnits(sourceName) = sortedUnits
          }

        }, 0)
      } catch {
        case e: Exception => {
          System.err.println("Error reading classfile.")
          e.printStackTrace(System.err)
        }
      }
      finally {
        fs.close()
      }
    }
    println("Finished parsing " + classFiles.length + " class files.")
  }
}

class DebugUnit(
  val startLine: Int,
  val endLine: Int,
  val classFile: File,
  val sourceName: String,
  val packageName: String,
  val classQualName: String) {

  override def toString = Map(
    "startLine" -> startLine,
    "endLine" -> endLine,
    "classFile" -> classFile,
    "sourceName" -> sourceName,
    "packageName" -> packageName,
    "classQualName" -> classQualName
    ).toString
}

