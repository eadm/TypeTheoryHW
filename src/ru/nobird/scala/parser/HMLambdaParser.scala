package ru.nobird.scala.parser

import ru.nobird.scala.expression.lambda._
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers

/**
  * Created by ruslandavletshin on 09/04/2017.
  */
object HMLambdaParser extends StdTokenParsers with PackratParsers  {
    class LambdaLexer extends StdLexical {
        override def letter: Parser[Char] = elem("letter", c => c.isLetter && c != '\\' && c != '=')
    }

    type Tokens = StdLexical
    val lexical = new LambdaLexer
    lexical.delimiters ++= Seq("=", "\\", ".", "(", ")")
    lexical.reserved ++= Seq("let", "in")



    lazy val expression: PackratParser[LambdaExpression] =
        "let" ~ variable ~ "=" ~ expression ~ "in" ~ expression ^^ {
            case "let" ~ v ~ "=" ~ e1 ~ "in" ~ e2 => new Let(v, e1, e2)
        } | abstraction

    lazy val abstraction: PackratParser[LambdaExpression] =
        application.? ~ lambda ^^ {
            case apO ~ l =>
                apO match {
                    case Some(ap: Application) =>
                        ap.pack(l)
                    case Some(ap) =>
                        new Application(ap, l)
                    case None => l
                }
        } | application

    lazy val lambda: PackratParser[Lambda] = "\\" ~> variable ~ "." ~ abstraction ^^ {
        case x ~ "." ~ v => new Lambda(x, v)
    }

    lazy val application: PackratParser[LambdaExpression] =
        application ~ term ^^ {
            case left ~ right =>
                right match {
                    case a: Application => a.pack(left)
                    case _ => new Application(left, right)
                }
        } | term

    lazy val term: PackratParser[LambdaExpression] = brackets | variable
    lazy val brackets: PackratParser[LambdaExpression] = "(" ~> expression <~ ")" ^^ (x => new Brackets(x))
    lazy val variable: PackratParser[Variable] = ident ^^ (x => new Variable(x))

    def parse(source: String): ParseResult[LambdaExpression] = {
        val tokens = new lexical.Scanner(source)
        phrase(expression)(tokens)
    }
}
