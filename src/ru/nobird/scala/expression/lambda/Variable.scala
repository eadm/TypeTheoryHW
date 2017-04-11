package ru.nobird.scala.expression.lambda

import ru.nobird.scala.expression.intuitionistic.{Equation, TypeExpression, TypeVariable}

/**
  * Created by ruslandavletshin on 21/06/16.
  */
case class Variable(x: String) extends LambdaExpression(x) {
    override def betaReduction(vars: Map[String, LambdaExpression]):LambdaExpression =
        vars.get(x) match {
            case Some(e:LambdaExpression) => e
            case _ => this
        }

    override val isBetaRedex    = false
    override val isInNormalForm = true

    override def escapeBrackets(): LambdaExpression = this

    override def getTypeAnnotation(cache: Map[String, TypeExpression]): (List[Equation], TypeExpression) =
        cache.get(x) match {
            case Some(t) => (List(), t)
            case _ =>
                val t = new TypeVariable(LambdaExpression.getNextTypeVar)
                (List(), t)
        }

}
