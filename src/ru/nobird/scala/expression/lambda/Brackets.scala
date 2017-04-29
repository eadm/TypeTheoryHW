package ru.nobird.scala.expression.lambda
import ru.nobird.scala.expression.intuitionistic.{Equation, TypeExpression}

/**
  * Created by ruslandavletshin on 03/03/2017.
  */
case class Brackets(e: LambdaExpression) extends LambdaExpression("(" + e + ")") {

    override def substitute(v: String, expr: LambdaExpression): LambdaExpression = e.substitute(v, expr)
    override def betaReduction(): LambdaExpression = e.betaReduction()

    override lazy val isInNormalForm: Boolean = e.isInNormalForm
    override lazy val isBetaRedex: Boolean = e.isBetaRedex

    override def getTypeAnnotation(cache: Map[String, TypeExpression]): (List[Equation], TypeExpression) = e.getTypeAnnotation(cache)
    override def escapeBrackets(): LambdaExpression = e.escapeBrackets()

    override def getAllVars: Set[String] = e.getAllVars

    override def rename(s: Map[String, String]): LambdaExpression = e.rename(s)
}
