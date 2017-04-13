package ru.nobird.scala.expression.lambda
import ru.nobird.scala.expression.intuitionistic.{Equation, TypeExpression}

/**
  * Created by ruslandavletshin on 09/04/2017.
  */
case class Let (x: Variable, a: LambdaExpression, b: LambdaExpression)
    extends LambdaExpression("let " + x + " = [" + a + "] in [" + b + "]") {
    override def betaReduction(vars: Map[String, LambdaExpression]): LambdaExpression = null

    override val isInNormalForm: Boolean = false
    override val isBetaRedex: Boolean = false

    override def getTypeAnnotation(cache: Map[String, TypeExpression]): (List[Equation], TypeExpression) = null

    override def escapeBrackets(): LambdaExpression = Let(x, a.escapeBrackets(), b.escapeBrackets())

    override def getAllVars: Set[String] = a.getAllVars ++ b.getAllVars + x.toString

    override def rename(s: Map[String, String]): LambdaExpression =
        Let(x.rename(s), a.rename(s), b.rename(s))
}
