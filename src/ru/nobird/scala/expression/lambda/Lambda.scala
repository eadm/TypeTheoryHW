package ru.nobird.scala.expression.lambda

import ru.nobird.scala.expression.intuitionistic.{Equation, TypeExpression, TypeVariable}

/**
  * Created by ruslandavletshin on 21/06/16.
  */
case class Lambda(var v: Variable, var ex: LambdaExpression) extends LambdaExpression("(\\" + v.toString + "." + ex.toString + ")") {

    override def copy(): LambdaExpression =
        Lambda(v.copy(), ex.copy())

    override def forceToString(): String =
        "(\\" + v.forceToString() + "." + ex.forceToString() + ")"

    override def substitute(variable: String, expr: LambdaExpression): LambdaExpression =
        if (variable == v.toString)
            this
        else {
            ex = ex.substitute(variable, expr)
            this
        }

    override def betaReduction(): Option[LambdaExpression] = ex.betaReduction() match {
        case Some(expr) =>
            ex = expr
            Some(this)
        case _ => None
    }



    override def escapeBrackets(): LambdaExpression =
        Lambda(v, ex.escapeBrackets())


    override def getTypeAnnotation(cache: Map[String, TypeExpression]): (List[Equation], TypeExpression) = {
        val tx = TypeVariable(LambdaExpression.getNextTypeVar)
        val (e, tp) = ex.getTypeAnnotation(cache + ((v.toString, tx)))
        (e, tx -> tp)
    }

    override def getAllVars: Set[String] = ex.getAllVars + v.forceToString()


    override def rename(s: Map[String, String]): LambdaExpression = {
        v.rename(s)
        ex.rename(s)
        this
    }
}
