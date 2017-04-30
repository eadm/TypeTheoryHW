package ru.nobird.scala.expression.lambda

import ru.nobird.scala.expression.intuitionistic.{Equation, TypeExpression, TypeVariable}

/**
  * Created by ruslandavletshin on 21/06/16.
  */
case class Variable(var x: String) extends LambdaExpression(x) {

    override def copy(): Variable = Variable(x)

    override def substitute(v: String, expr: LambdaExpression): LambdaExpression =
        if (v == x)
            expr.copy()
        else
            this

    override def betaReduction(): Option[LambdaExpression] = None

    override def escapeBrackets(): LambdaExpression = this

    override def getAllVars: Set[String] = Set()

    override def rename(s: Map[String, String]): Variable = s.get(x) match {
        case Some(x1: String) =>
            x = x1
            this
        case _ => this
    }

    override def getTypeAnnotation(cache: Map[String, TypeExpression]): (List[Equation], TypeExpression) =
        cache.get(x) match {
            case Some(t) => (List(), t)
            case _ =>
                val t = TypeVariable(LambdaExpression.getNextTypeVar)
                (List(), t)
        }

    override def forceToString(): String = x
}
