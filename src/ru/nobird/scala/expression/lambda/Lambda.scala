package ru.nobird.scala.expression.lambda

import ru.nobird.scala.expression.intuitionistic.{Equation, Implication, TypeExpression, TypeVariable}

/**
  * Created by ruslandavletshin on 21/06/16.
  */
class Lambda(val v: Variable, ex: LambdaExpression) extends LambdaExpression("(\\" + v.toString + "." + ex.toString + ")") {

    override def betaReduction(vars: Map[String, LambdaExpression]): LambdaExpression =
        if (vars.contains(v.toString))
            if (vars.contains(v.toString + " _"))
                new Lambda(v, ex.betaReduction(Map()))
            else
                ex.betaReduction(vars + ((v.toString + " _", null)))
        else
            new Lambda(v, ex.betaReduction(vars))



    override def escapeBrackets(): LambdaExpression =
        new Lambda(v, ex.escapeBrackets())


    override lazy val isInNormalForm: Boolean = ex.isInNormalForm
    override val isBetaRedex = false

    override def getTypeAnnotation(cache: Map[String, TypeExpression]): (List[Equation], TypeExpression) = {
        val tt = /*cache.get(v.toString) match {
            case Some(t) => t
            case _ => */new TypeVariable(LambdaExpression.getNextTypeVar) // todo: consider to use only this for tracking context
//        }
        val e = ex.getTypeAnnotation(cache + ((v.toString, tt)))
        (e._1, new Implication(tt, e._2))
    }
}
