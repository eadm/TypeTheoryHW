package ru.nobird.scala.hw

import ru.nobird.scala.parser.UntypedLambdaParser
import UntypedLambdaParser.{NoSuccess, Success}
import ru.nobird.scala.expression.lambda.LambdaExpression

class HW2 extends HW {
    override def solve(in: String): String =
        UntypedLambdaParser.parse(in) match {
            case Success(expr, _) =>
                LambdaExpression.inferenceType(expr.escapeBrackets()) match {
                    case Some(t) => t.toString
                    case None => "Лямбда-выражение не имеет типа."
                }
            case err: NoSuccess => err.toString
        }
}
