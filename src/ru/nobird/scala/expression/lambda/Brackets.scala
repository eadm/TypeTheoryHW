package ru.nobird.scala.expression.lambda
import ru.nobird.scala.expression.intuitionistic.{Equation, TypeExpression}

/**
  * Created by ruslandavletshin on 03/03/2017.
  */
class Brackets(e: LambdaExpression) extends LambdaExpression("(" + e + ")") {
    override def betaReduction(vars: Map[String, LambdaExpression]): LambdaExpression = e.betaReduction(vars)

    override lazy val isInNormalForm: Boolean = e.isInNormalForm
    override lazy val isBetaRedex: Boolean = e.isBetaRedex

    override def getTypeAnnotation(cache: Map[String, TypeExpression]): (List[Equation], TypeExpression) = e.getTypeAnnotation(cache)
    override def escapeBrackets(): LambdaExpression = e.escapeBrackets()
}
