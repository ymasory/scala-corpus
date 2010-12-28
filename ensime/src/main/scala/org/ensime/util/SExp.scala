package org.ensime.util
import scala.collection.immutable.Map
import scala.util.parsing.combinator._
import scala.util.parsing.input._

abstract class SExp extends WireFormat {
  def toReadableString: String = toString
  override def toWireString: String = toReadableString
  def toScala: Any = toString
}

case class SExpList(items: Iterable[SExp]) extends SExp with Iterable[SExp] {

  override def iterator = items.iterator

  override def toString = "(" + items.mkString(" ") + ")"

  override def toReadableString = {
    "(" + items.map { _.toReadableString }.mkString(" ") + ")"
  }

  def toKeywordMap(): Map[KeywordAtom, SExp] = {
    var m = Map[KeywordAtom, SExp]()
    items.sliding(2, 2).foreach {
      case (key: KeywordAtom) ::(sexp: SExp) :: rest => {
        m += (key -> sexp)
      }
      case _ => {}
    }
    m
  }

  def toSymbolMap(): Map[scala.Symbol, Any] = {
    var m = Map[scala.Symbol, Any]()
    items.sliding(2, 2).foreach {
      case SymbolAtom(key) ::(sexp: SExp) :: rest => {
        m += (Symbol(key) -> sexp.toScala)
      }
      case _ => {}
    }
    m
  }
}

object BooleanAtom {

  def unapply(z: SExp): Option[Boolean] = z match {
    case TruthAtom() => Some(true)
    case NilAtom() => Some(false)
    case _ => None
  }

}

abstract class BooleanAtom extends SExp {
  def toBool: Boolean
  override def toScala = toBool
}

case class NilAtom() extends BooleanAtom {
  override def toString = "nil"
  override def toBool: Boolean = false

}
case class TruthAtom() extends BooleanAtom {
  override def toString = "t"
  override def toBool: Boolean = true
  override def toScala: Boolean = true
}
case class StringAtom(value: String) extends SExp {
  override def toString = value
  override def toReadableString = {
    val printable = value.replace("\\", "\\\\").replace("\"", "\\\"");
    "\"" + printable + "\""
  }
}
case class IntAtom(value: Int) extends SExp {
  override def toString = String.valueOf(value)
  override def toScala = value
}
case class SymbolAtom(value: String) extends SExp {
  override def toString = value
}
case class KeywordAtom(value: String) extends SExp {
  override def toString = value
}

object SExp extends RegexParsers {

  import scala.util.matching.Regex

  lazy val string = regexGroups("""\"((?:[^\"\\]|\\.)*)\"""".r) ^^ { m => StringAtom(m.group(1)) }
  lazy val sym = regex("[a-zA-Z][a-zA-Z0-9-:]*".r) ^^ SymbolAtom
  lazy val keyword = regex(":[a-zA-Z][a-zA-Z0-9-:]*".r) ^^ KeywordAtom
  lazy val number = regex("[0-9]+".r) ^^ { cs => IntAtom(cs.toInt) }
  lazy val list = literal("(") ~> rep(expr) <~ literal(")") ^^ SExpList.apply
  lazy val nil = literal("nil") ^^ { cs => NilAtom() }
  lazy val truth = literal("t") ^^ { cs => TruthAtom() }
  lazy val expr: Parser[SExp] = list | nil | truth | keyword | sym | number | string

  def read(r: Reader[Char]): SExp = {
    val result: ParseResult[SExp] = expr(r)
    result match {
      case Success(value, next) => value
      case Failure(errMsg, next) => {
        println(errMsg)
        NilAtom()
      }
      case Error(errMsg, next) => {
        println(errMsg)
        NilAtom()
      }
    }
  }

  /** A parser that matches a regex string and returns the match groups */
  def regexGroups(r: Regex): Parser[Regex.Match] = new Parser[Regex.Match] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      (r findPrefixMatchOf (source.subSequence(start, source.length))) match {
        case Some(matched) => Success(matched, in.drop(start + matched.end - offset))
        case None => Failure("string matching regex `" + r +
          "' expected but `" +
          in.first + "' found", in.drop(start - offset))
      }
    }
  }

  def apply(items: SExp*): SExpList = {
    SExpList(items)
  }

  def apply(items: Iterable[SExp]): SExpList = {
    SExpList(items)
  }

  // Helpers for common case of key,val prop-list.
  // Omit keys for nil values.
  def propList(items: (String, SExp)*): SExpList = {
    propList(items)
  }
  def propList(items: Iterable[(String, SExp)]): SExpList = {
    val nonNil = items.filter {
      case (s, NilAtom()) => false
      case (s, SExpList(items)) if items.isEmpty => false
      case _ => true
    }
    SExpList(nonNil.flatMap(ea => List(key(ea._1), ea._2)))
  }

  implicit def strToSExp(str: String): SExp = {
    StringAtom(str)
  }

  def key(str: String): KeywordAtom = {
    KeywordAtom(str)
  }

  implicit def intToSExp(value: Int): SExp = {
    IntAtom(value)
  }

  implicit def boolToSExp(value: Boolean): SExp = {
    if (value) {
      TruthAtom()
    } else {
      NilAtom()
    }
  }

  implicit def symbolToSExp(value: Symbol): SExp = {
    if (value == 'nil) {
      NilAtom()
    } else {
      SymbolAtom(value.toString.drop(1))
    }
  }

  implicit def nilToSExpList(nil: NilAtom): SExp = {
    SExpList(List())
  }

  implicit def toSExp(o: SExpable): SExp = {
    o.toSExp
  }

  implicit def toSExpable(o: SExp): SExpable = new SExpable {
    def toSExp = o
  }

  implicit def listToSExpable(o: Iterable[SExpable]): SExpable = new Iterable[SExpable] with SExpable {
    override def iterator = o.iterator
    override def toSExp = SExp(o.map { _.toSExp })
  }

}

abstract trait SExpable {
  implicit def toSExp(): SExp
}

