package ru.nobird.scala.expression.lambda

import ru.nobird.scala.expression.intuitionistic.{Equation, TypeExpression, TypeVariable}

/**
  * Created by ruslandavletshin on 21/06/16.
  */
case class Lambda(v: Variable, ex: LambdaExpression) extends LambdaExpression("(\\" + v.toString + "." + ex.toString + ")") {


    override def substitute(variable: String, expr: LambdaExpression): LambdaExpression =
        if (variable == v.toString)
            this
        else
            Lambda(v, ex.substitute(variable, expr))

    override def betaReduction(): Option[LambdaExpression] = ex.betaReduction() match {
        case Some(expr) => Some(Lambda(v, expr))
        case _ => None
    }



    override def escapeBrackets(): LambdaExpression =
        Lambda(v, ex.escapeBrackets())


    override def getTypeAnnotation(cache: Map[String, TypeExpression]): (List[Equation], TypeExpression) = {
        val tx = TypeVariable(LambdaExpression.getNextTypeVar)
        val (e, tp) = ex.getTypeAnnotation(cache + ((v.toString, tx)))
        (e, tx -> tp)
    }

    override def getAllVars: Set[String] = ex.getAllVars + v.toString

    override def rename(s: Map[String, String]): LambdaExpression =
        Lambda(v.rename(s), ex.rename(s))
}
