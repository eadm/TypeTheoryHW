package ru.nobird.scala.expression.lambda

import ru.nobird.scala.expression.intuitionistic.{Equation, TypeExpression, TypeVariable}

case class Application(left: LambdaExpression, right: LambdaExpression) extends LambdaExpression("(" + left + " "  + " " + right + ")") {


    override def substitute(v: String, expr: LambdaExpression): LambdaExpression =
        Application(left.substitute(v, expr), right.substitute(v, expr))

    override def betaReduction(): LambdaExpression = left match {
        case Lambda(v, ex) =>
            val collision = left.getAllVars & right.getAllVars - v.toString
            val s = collision.map{x => (x, LambdaExpression.getNextTypeVar)}.toMap
            ex.substitute(v.toString, right.rename(s))

        case _ =>
            val l = left.betaReduction()
            if (l.toString == left.toString)
                Application(l, right.betaReduction())
            else
                Application(l, right)
    }



    override def escapeBrackets(): Application = Application(left.escapeBrackets(), right.escapeBrackets())

    override lazy val isBetaRedex: Boolean = left.isInstanceOf[Lambda]
    override lazy val isInNormalForm: Boolean = !isBetaRedex && left.isInNormalForm && right.isInNormalForm

    def pack(e: LambdaExpression): Application = left match { // to maintain left associativity of application
        case a: Application => Application(a.pack(e), right)
        case _ => Application(Application(e, left), right)
    }

    override def getTypeAnnotation(cache: Map[String, TypeExpression]): (List[Equation], TypeExpression) = {
        val l = left.getTypeAnnotation(cache)
        val r = right.getTypeAnnotation(cache)

        val ty = TypeVariable(LambdaExpression.getNextTypeVar)

        (l._1 ++ r._1 :+ Equation(l._2, r._2 -> ty), ty)
    }

    override def getAllVars: Set[String] = left.getAllVars ++ right.getAllVars

    override def rename(s: Map[String, String]): LambdaExpression =
        Application(left.rename(s), right.rename(s))
}
