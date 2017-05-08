package ru.nobird.scala.hw

import ru.nobird.scala.expression.hm.{Context, Substitution, TypeInferenceException}
import ru.nobird.scala.expression.lambda._
import ru.nobird.scala.parser.HMLambdaParser
import HMLambdaParser.{NoSuccess, Success}
import ru.nobird.scala.expression.intuitionistic._

import scala.collection.immutable.HashMap

/**
  * Created by ruslandavletshin on 10/04/2017.
  */
class HW3 extends HW {
    override def solve(in: String): String =
        HMLambdaParser.parse(in) match {
            case Success(expr, _) =>
                try {
                    val (ctx, t) = W(Context(HashMap[String, TypeExpression]()), expr.escapeBrackets())
                    ctx(t).toString
                } catch {
                    case _: TypeInferenceException => "Лямбда-выражение не имеет типа."
                }
            case err: NoSuccess => err.toString
        }


    private def W(ctx: Context, expr: LambdaExpression): (Substitution, TypeExpression) =
        expr match {

            case Application(e1, e2) =>
                val (s1, t1) = W(ctx, e1)
                val (s2, t2) = W(s1(ctx), e2)

                val beta = TypeVariable(LambdaExpression.getNextTypeVar)
                val v = unify(s2(t1), t2 -> beta)
                val s = v(s1(s2))

                (s, s(beta))


            case Lambda(x, e) =>
                val beta = TypeVariable(LambdaExpression.getNextTypeVar)
                val (s1, t1) = W(ctx - x.toString + (x.toString, beta), e)

                (s1, s1(beta) -> t1)

            case Variable(x) =>
                ctx.get(x) match {
                    case Some(t) =>
                        val vars = t.getVars -- t.getFreeVars
                        val tt = t.unwrap()
                        (new Substitution,
                            Substitution(vars.map { _ -> TypeVariable(LambdaExpression.getNextTypeVar)}.toMap)(tt))


                    case None =>
                        throw new TypeInferenceException("There is no type in Context for variable " + x)
                }

            case Let(x, e1, e2) =>
                val (s1, t1) = W(ctx, e1)
                val (s2, t2) = W(s1(ctx - x.toString) + (x.toString, s1(ctx.closure(t1))) , e2)

                (s2(s1), t2)
        }



    private def unify(t1: TypeExpression, t2: TypeExpression): Substitution = {
        val ss = new EquationSystem(List(Equation(t1, t2)))
        LambdaExpression.resolveEquationSystem(ss) match {
            case Some(sub) => Substitution(sub)
            case _ => throw new TypeInferenceException("can't solve equation system")
        }
    }


}
