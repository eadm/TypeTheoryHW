package ru.nobird.scala.hw

import ru.nobird.scala.expression.lambda.LambdaExpression
import ru.nobird.scala.parser.UntypedLambdaParser
import UntypedLambdaParser.{Success, NoSuccess}

class HW1 extends HW {
    override def solve(in: String): String =
        UntypedLambdaParser.parse(in) match {
            case Success(expr, _) => LambdaExpression.normalise(expr.escapeBrackets()).toString
            case err: NoSuccess => err.toString
        }
}
