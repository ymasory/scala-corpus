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

import scala.reflect.Manifest
import JsonAST._
import JsonParser.parse

/** Functions to serialize and deserialize a case class.
 * Custom serializer can be inserted if a class is not a case class.
 * <p>
 * Example:<pre>
 * val hints = new ShortTypeHints( ... )
 * implicit val formats = Serialization.formats(hints)
 * </pre>
 *
 * @see blueeyes.json.TypeHints
 */
object Serialization {
  import java.io.{Reader, StringWriter, Writer}

  /** Serialize to String.
   */
  def write[A <: AnyRef](a: A)(implicit formats: Formats): String =
    (write(a, new StringWriter)(formats)).toString

  /** Serialize to Writer.
   */
  def write[A <: AnyRef, W <: Writer](a: A, out: W)(implicit formats: Formats): W =
    Printer.compact(render(Extraction.decompose(a)(formats)), out)

  /** Deserialize from a String.
   */
  def read[A](json: String)(implicit formats: Formats, mf: Manifest[A]): A =
    parse(json).extract(formats, mf)

  /** Deserialize from a Reader.
   */
  def read[A, R <: Reader](in: Reader)(implicit formats: Formats, mf: Manifest[A]): A =
    parse(in).extract(formats, mf)

  /** Create Serialization formats with given type hints.
   * <p>
   * Example:<pre>
   * val hints = new ShortTypeHints( ... )
   * implicit val formats = Serialization.formats(hints)
   * </pre>
   */
  def formats(hints: TypeHints) = new Formats {
    val dateFormat = DefaultFormats.lossless.dateFormat
    override val typeHints = hints
  }
}

}
}
