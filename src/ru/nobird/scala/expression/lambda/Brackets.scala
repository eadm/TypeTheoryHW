package ru.nobird.scala.expression.lambda
import ru.nobird.scala.expression.intuitionistic.{Equation, TypeExpression}

/**
  * Created by ruslandavletshin on 03/03/2017.
  */
case class Brackets(e: LambdaExpression) extends LambdaExpression("(" + e + ")") {

    override def copy(): LambdaExpression = e.copy()

    override def forceToString(): String = "(" + e.forceToString() + ")"

    override def substitute(v: String, expr: LambdaExpression): LambdaExpression = e.substitute(v, expr)
    override def betaReduction(): Option[LambdaExpression] = e.betaReduction()

    override def getTypeAnnotation(cache: Map[String, TypeExpression]): (List[Equation], TypeExpression) = e.getTypeAnnotation(cache)
    override def escapeBrackets(): LambdaExpression = e.escapeBrackets()

    override def getAllVars: Set[String] = e.getAllVars

    override def rename(s: Map[String, String]): LambdaExpression = e.rename(s)
}
