package ru.nobird.scala.expression.lambda

import ru.nobird.scala.expression.intuitionistic.{Equation, TypeExpression, TypeVariable}

case class Application(var left: LambdaExpression, var right: LambdaExpression) extends LambdaExpression("(" + left + " "  + " " + right + ")") {

    override def forceToString(): String =
        "(" + left.forceToString() + " "  + " " + right.forceToString() + ")"

    override def substitute(v: String, expr: LambdaExpression): LambdaExpression = {
        left = left.substitute(v, expr)
        right = right.substitute(v, expr)
        this
    }
//        Application(left.substitute(v, expr), right.substitute(v, expr))

    override def betaReduction(): Option[LambdaExpression] = left match {
        case Lambda(v, ex) =>
            val collision = left.getAllVars & right.getAllVars - v.toString
            val s = collision.map{x => (x, LambdaExpression.getNextTypeVar)}.toMap
            Some(ex.substitute(v.toString, right.rename(s)))

        case _ =>
            left.betaReduction() match {
                case Some(l) =>
                    left = l
                    Some(this)
//                    Some(Application(l, right))
                case _ =>
                    right.betaReduction() match {
                        case Some(r) =>
                            right = r
                            Some(this)
//                            Some(Application(left, r))
                        case _ => None
                    }
            }
    }



    override def escapeBrackets(): Application = Application(left.escapeBrackets(), right.escapeBrackets())

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
