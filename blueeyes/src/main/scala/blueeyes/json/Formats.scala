/*
 * Copyright 2009-2010 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package blueeyes {
package json {

import java.util.{Date, TimeZone}
import JsonAST.{JObject, JValue}

/** Formats to use when converting JSON.
 * Formats are usually configured by using an implicit parameter:
 * <pre>
 * implicit val formats = blueeyes.json.DefaultFormats
 * </pre>
 */
trait Formats { self: Formats =>
  val dateFormat: DateFormat
  val typeHints: TypeHints = NoTypeHints
  val customSerializers: List[Serializer[_]] = Nil
  
  /**
   * Adds the specified type hints to this formats.
   */
  def + (extraHints: TypeHints): Formats = new Formats {
    val dateFormat = Formats.this.dateFormat
    override val typeHints = self.typeHints + extraHints
    override val customSerializers = self.customSerializers
  }

  /**
   * Adds the specified custom serializer to this formats.
   */
  def + (newSerializer: Serializer[_]): Formats = new Formats {
    val dateFormat = Formats.this.dateFormat
    override val typeHints = self.typeHints
    override val customSerializers = newSerializer :: self.customSerializers
  }

  def customSerializer(implicit format: Formats) = 
    customSerializers.foldLeft(Map(): PartialFunction[Any, JValue]) { (acc, x) => 
      acc.orElse(x.serialize) 
    }

  def customDeserializer(implicit format: Formats) = 
    customSerializers.foldLeft(Map(): PartialFunction[(TypeInfo, JValue), Any]) { (acc, x) => 
      acc.orElse(x.deserialize) 
    }
}

/** Conversions between String and Date.
 */
trait DateFormat {
  def parse(s: String): Option[Date]
  def format(d: Date): String
}

trait Serializer[A] {
  def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), A]
  def serialize(implicit format: Formats): PartialFunction[Any, JValue]
}

/** Type hints can be used to alter the default conversion rules when converting
 * Scala instances into JSON and vice versa. Type hints must be used when converting
 * class which is not supported by default (for instance when class is not a case class).
 * <p>
 * Example:<pre>
 * class DateTime(val time: Long)
 *
 * val hints = new ShortTypeHints(classOf[DateTime] :: Nil) {
 *   override def serialize: PartialFunction[Any, JObject] = {
 *     case t: DateTime => JObject(JField("t", JInt(t.time)) :: Nil)
 *   }
 *
 *   override def deserialize: PartialFunction[(String, JObject), Any] = {
 *     case ("DateTime", JObject(JField("t", JInt(t)) :: Nil)) => new DateTime(t.longValue)
 *   }
 * }
 * implicit val formats = DefaultFormats.withHints(hints)
 * </pre>
 */
trait TypeHints {  
  val hints: List[Class[_]]
  
  /** Return hint for given type.
   */
  def hintFor(clazz: Class[_]): String

  /** Return type for given hint.
   */
  def classFor(hint: String): Option[Class[_]]

  def containsHint_?(clazz: Class[_]) = hints exists (_ isAssignableFrom clazz)
  def deserialize: PartialFunction[(String, JObject), Any] = Map()
  def serialize: PartialFunction[Any, JObject] = Map()
  
  def components: List[TypeHints] = List(this)
  
  /**
   * Adds the specified type hints to this type hints.
   */
  def + (hints: TypeHints): TypeHints = CompositeTypeHints(hints.components ::: components)
  
  private[TypeHints] case class CompositeTypeHints(override val components: List[TypeHints]) extends TypeHints {
    val hints: List[Class[_]] = components.flatMap(_.hints)

    /**
     * Chooses most specific class.
     */
    def hintFor(clazz: Class[_]): String = components.filter(_.containsHint_?(clazz)).
        map(th => (th.hintFor(clazz), th.classFor(th.hintFor(clazz)).getOrElse(error("hintFor/classFor not invertible for " + th)))).
          sortWith((x, y) => delta(x._2, clazz) - delta(y._2, clazz) < 0).head._1

    def classFor(hint: String): Option[Class[_]] = hints find (hintFor(_) == hint)

    override def deserialize: PartialFunction[(String, JObject), Any] = components.foldLeft[PartialFunction[(String, JObject),Any]](Map()) {
      (result, cur) => result.orElse(cur.deserialize)
    }

    override def serialize: PartialFunction[Any, JObject] = components.foldLeft[PartialFunction[Any, JObject]](Map()) {
      (result, cur) => result.orElse(cur.serialize)
    }
    
    private def delta(class1: Class[_], class2: Class[_]): Int = {
      if (class1 == class2) 0
      else if (class1.getInterfaces.contains(class2)) 0
      else if (class2.getInterfaces.contains(class1)) 0
      else if (class1.isAssignableFrom(class2)) {
        1 + delta(class1, class2.getSuperclass)
      }
      else if (class2.isAssignableFrom(class1)) {
        1 + delta(class1.getSuperclass, class2)
      }
      else error("Don't call delta unless one class is assignable from the other")
    }
  }
}

/** Do not use any type hints.
 */
case object NoTypeHints extends TypeHints {
  val hints = Nil
  def hintFor(clazz: Class[_]) = error("NoTypeHints does not provide any type hints.")
  def classFor(hint: String) = None
}

/** Use short class name as a type hint.
 */
case class ShortTypeHints(hints: List[Class[_]]) extends TypeHints {
  def hintFor(clazz: Class[_]) = clazz.getName.substring(clazz.getName.lastIndexOf(".")+1)
  def classFor(hint: String) = hints find (hintFor(_) == hint)
}

/** Use full class name as a type hint.
 */
case class FullTypeHints(hints: List[Class[_]]) extends TypeHints {
  def hintFor(clazz: Class[_]) = clazz.getName
  def classFor(hint: String) = Some(Thread.currentThread.getContextClassLoader.loadClass(hint))
}

/** Default date format is UTC time.
 */
object DefaultFormats extends DefaultFormats {
  val losslessDate = new ThreadLocal(new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'"))
  val UTC = TimeZone.getTimeZone("UTC")
}

trait DefaultFormats extends Formats {
  import java.text.{ParseException, SimpleDateFormat}

  val dateFormat = new DateFormat {    
    def parse(s: String) = try {
      Some(formatter.parse(s))
    } catch {
      case e: ParseException => None
    }
    
    def format(d: Date) = formatter.format(d)

    private def formatter = {
      val f = dateFormatter
      f.setTimeZone(DefaultFormats.UTC)
      f
    }
  }

  protected def dateFormatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")

  /** Lossless date format includes milliseconds too.
   */
  def lossless = new DefaultFormats {
    override def dateFormatter = DefaultFormats.losslessDate()
  }

  /** Default formats with given <code>TypeHint</code>s.
   */
  def withHints(hints: TypeHints) = new DefaultFormats {
    override val typeHints = hints
  }
}

private[json] class ThreadLocal[A](init: => A) extends java.lang.ThreadLocal[A] with (() => A) {
  override def initialValue = init
  def apply = get
}

}
}
