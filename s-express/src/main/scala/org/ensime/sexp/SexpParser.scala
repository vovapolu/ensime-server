// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.sexp

import org.parboiled2._

import scala.util.{ Failure, Success }

/**
 * Parse Emacs Lisp into an `Sexp`. Other lisp variants may
 * require tweaking, e.g. Scheme's nil, infinity, NaN, etc.
 */
object SexpParser {

  def parse(desc: String): Sexp = {
    val parser = new SexpParser(desc)
    parser.SexpP.run() match {
      case Success(d) =>
        d
      case Failure(error: ParseError) =>
        val msg = parser.formatError(error, new ErrorFormatter(showTraces = true))
        throw new Exception("Failed to parse sexp: " + msg)
      case Failure(other) =>
        throw new Exception("Failed to parse sexp: ", other)
    }
  }

  // https://www.gnu.org/software/emacs/manual/html_node/elisp/Basic-Char-Syntax.html
  // https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-for-Strings.html
  // Not supported: https://www.gnu.org/software/emacs/manual/html_node/elisp/Non_002dASCII-in-Strings.html
  private[sexp] val specialChars = Map[String, String](
    "\"" -> "\"",
    "a" -> 7.toChar.toString,
    "b" -> "\b",
    "t" -> "\t",
    "n" -> "\n",
    "v" -> 11.toChar.toString,
    "f" -> "\f",
    "r" -> "\r",
    "e" -> 27.toChar.toString,
    "s" -> " ",
    "d" -> 127.toChar.toString,
    "\\" -> "\\"
  )
}

/**
 * Parse Emacs Lisp into an `Sexp`. Other lisp variants may
 * require tweaking, e.g. Scheme's nil, infinity, NaN, etc.
 */
class SexpParser(val input: ParserInput) extends Parser with StringBuilding {

  private def SexpP: Rule1[Sexp] = rule {
    SexpAtomP | SexpListP | SexpEmptyList | SexpConsP | SexpQuotedP
  }

  private def SexpConsP: Rule1[SexpCons] = rule {
    LeftBrace ~ SexpP ~ Whitespace ~ "." ~ Whitespace ~ SexpP ~ RightBrace ~> {
      (x: Sexp, y: Sexp) => SexpCons(x, y)
    }
  }

  private def SexpListP: Rule1[Sexp] = rule {
    LeftBrace ~ SexpP ~ zeroOrMore(Whitespace ~ SexpP) ~ RightBrace ~> {
      (head: Sexp, tail: Seq[Sexp]) => { SexpList(head :: tail.toList) }
    }
  }

  private def SexpAtomP: Rule1[SexpAtom] = rule {
    SexpCharP | SexpStringP | SexpNaNP | SexpNumberP | SexpSymbolP
  }

  private def SexpCharP: Rule1[SexpChar] = rule {
    '?' ~ NormalChar ~> { (c: String) => SexpChar(c.head) }
  }

  private def SexpStringP: Rule1[SexpString] = rule {
    '"' ~ zeroOrMore(Character) ~ '"' ~> { strs: Seq[String] => SexpString(strs.mkString("")) }
    //    '"' ~ clearSB() ~ zeroOrMore((Character | "\"\"") ~ appendSB()) ~ '"' ~ push(SexpString(sb.toString))
  }

  def SexpNumberP = rule {
    capture(Integer ~ optional(Frac) ~ optional(Exp)) ~> { s: String => SexpNumber(BigDecimal(s)) }
  }

  import CharPredicate.{ Alpha, Digit, Digit19 }

  def Integer = rule {
    optional('-') ~ (Digit19 ~ Digits | Digit)
  }

  def Digits = rule {
    oneOrMore(Digit)
  }

  def Frac = rule {
    "." ~ Digits
  }

  def Exp = rule {
    ignoreCase('e') ~ optional(anyOf("+-")) ~ Digits
  }

  private def SexpNaNP: Rule1[SexpAtom] = rule {
    "-1.0e+INF" ~ push(SexpNegInf) |
      "1.0e+INF" ~ push(SexpPosInf) |
      optional('-') ~ "0.0e+NaN" ~ push(SexpNaN)
  }

  private def SexpQuotedP: Rule1[Sexp] = rule {
    "'" ~ SexpP ~> { v: Sexp => SexpCons(SexpQuote, v) }
  }

  private def SexpSymbolP: Rule1[SexpAtom] = rule {
    // ? allowed at the end of symbol names
    capture(oneOrMore(Alpha | Digit | SymbolSpecial) ~ zeroOrMore(Alpha | Digit | SymbolSpecial | ".") ~ optional("?")) ~> { sym: String =>
      if (sym == "nil") SexpNil
      else SexpSymbol(sym)
    }
  }

  private def SexpEmptyList: Rule1[SexpNil.type] = rule {
    LeftBrace ~ RightBrace ~ push(SexpNil)
  }

  private val SexpQuote = SexpSymbol("quote")

  private def Character: Rule1[String] = rule {
    EscapedChar | NormalChar
  }

  private def EscapedChar: Rule1[String] = rule {
    '\\' ~ capture(ANY) ~> { s: String => unescape(s) }
  }

  private def NormalChar: Rule1[String] = rule {
    capture(CharPredicate.Printable -- "\"\\") ~> { s: String => s }
  }

  private def SymbolSpecial = rule {
    anyOf("+-*/_~!@$%^&=:<>{}")
  }

  private def Whitespace: Rule0 = rule {
    zeroOrMore(Comment | anyOf(" \n\r\t\f"))
  }

  private def Comment: Rule0 = rule {
    ";" ~ zeroOrMore(noneOf("\n")) ~ ("\n" | EOI)
  }

  private def LeftBrace: Rule0 = rule {
    Whitespace ~ '(' ~ Whitespace
  }

  private def RightBrace: Rule0 = rule {
    Whitespace ~ ')' ~ Whitespace
  }

  private val ignore = Set("\n", " ")

  private def unescape(c: String): String = {
    if (ignore(c)) ""
    else {
      val unescaped = SexpParser.specialChars.get(c)
      require(unescaped.isDefined, c + " is not a valid escaped character")
      unescaped.get
    }
  }
}
