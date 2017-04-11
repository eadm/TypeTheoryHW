package ru.nobird.scala.parser

import ru.nobird.scala.expression.lambda._

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers

/**
  * Created by ruslandavletshin on 21/06/16.
  */

object UntypedLambdaParser extends StdTokenParsers with PackratParsers {
    class LambdaLexer extends StdLexical {
        override def letter: Parser[Char] = elem("letter", c => c.isLetter && c != '\\')
    }

    type Tokens = StdLexical
    val lexical = new LambdaLexer
    lexical.delimiters ++= Seq("\\", ".", "(", ")")

    lazy val expr: PackratParser[LambdaExpression]     = lambda | application | variable | brackets

    lazy val lambda: PackratParser[Lambda]             = "\\" ~> variable ~ "." ~ expr ^^ { case v ~ "." ~ e  => Lambda(v, e) }

    lazy val application: PackratParser[Application]   = expr ~ expr ^^ {
        case left ~ right =>
            right match {
                case a: Application => a.pack(left)
                case _ => new Application(left, right)
            }
    }

    lazy val variable: PackratParser[Variable]         = ident ^^ Variable

    lazy val brackets: PackratParser[LambdaExpression] = "(" ~> expr <~ ")" ^^ Brackets


    def parse(source: String): ParseResult[LambdaExpression] = {
        val tokens = new lexical.Scanner(source)
        phrase(expr)(tokens)
    }
}
