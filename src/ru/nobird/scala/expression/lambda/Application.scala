package ru.nobird.scala.expression.lambda

import ru.nobird.scala.expression.intuitionistic.{Equation, Implication, TypeExpression, TypeVariable}

/**
  * Created by ruslandavletshin on 21/06/16.
  */
class Application(left: LambdaExpression, right: LambdaExpression) extends LambdaExpression("" + left + " "  + " " + right + "") {

    override def betaReduction(vars: Map[String, LambdaExpression]): LambdaExpression = (left, vars.isEmpty) match {
        case (lambda: Lambda, true) => left.betaReduction(Map((lambda.v.toString, right)))
        case (_, true) =>
            val l = left.betaReduction(vars)
            if (l.toString == left.toString)
                new Application(l, right.betaReduction(vars))
            else
                new Application(l, right)
        case (_, false) => new Application(left.betaReduction(vars), right.betaReduction(vars))
    }



    override def escapeBrackets(): Application = new Application(left.escapeBrackets(), right.escapeBrackets())

    override lazy val isBetaRedex: Boolean = left.isInstanceOf[Lambda]
    override lazy val isInNormalForm: Boolean = !isBetaRedex && left.isInNormalForm && right.isInNormalForm

    def pack(e: LambdaExpression): Application = left match { // to maintain left associativity of application
        case a: Application => new Application(a.pack(e), right)
        case _ => new Application(new Application(e, left), right)
    }

    override def getTypeAnnotation(cache: Map[String, TypeExpression]): (List[Equation], TypeExpression) = {
        val l = left.getTypeAnnotation(cache)
        val r = right.getTypeAnnotation(cache)

        val ty = new TypeVariable(LambdaExpression.getNextTypeVar)

        (l._1 ++ r._1 :+ new Equation(l._2, new Implication(r._2, ty)), ty)
    }
}
