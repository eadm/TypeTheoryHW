package ru.nobird.scala.expression.lambda

import ru.nobird.scala.expression.Expression
import ru.nobird.scala.expression.intuitionistic.{Equation, EquationSystem, TypeExpression}


abstract class LambdaExpression(s: String) extends Expression(s) {
    def substitute(v: String, expr: LambdaExpression): LambdaExpression

    def betaReduction(): Option[LambdaExpression]

    def getTypeAnnotation(cache: Map[String, TypeExpression]): (List[Equation], TypeExpression)

    def escapeBrackets(): LambdaExpression

    def getAllVars: Set[String]

    def copy() : LambdaExpression

    def rename(s: Map[String, String]): LambdaExpression

    def forceToString() : String
}

object LambdaExpression {
    def normalise(arg: LambdaExpression): LambdaExpression = {
        var e = arg
        var cc = 0
        var isNormalised = false
        while (!isNormalised) {
            e.betaReduction() match {
                case Some(expr) =>
                    e = expr
                    cc += 1
                    if (cc % 1000 == 0) println(cc / 1000 + "k")
                case _ => isNormalised = true
            }
        }
        e
    }

    private var count = -1

    def getNext: Int = {
        count = count + 1
        count
    }

    val alph = Array("𝛂", "𝛃", "𝛄", "𝛅", "𝛆", "𝛇", "𝛈", "𝛉", "𝛊",
        "𝛋", "𝛌", "𝛍", "𝛎", "𝛏", "𝛐", "𝛑", "𝛒", "𝛓", "𝛔", "𝛕", "𝛖", "𝛗", "𝛘", "𝛚", "𝛙")

    def getNextTypeVar: String = "a_" + getNext
//    def getNextTypeVar: String = alph(getNext)


    def resolveEquationSystem(s: EquationSystem): Option[Map[String, TypeExpression]] =
        if (s.isSolved) {
            Some(s.vars)
        } else {
            s.next() match {
                case Some(r) => {
//                    println("==========================")
//                    println(r)
                    resolveEquationSystem(r)
                }
                case None => None
            }
        }

    def inferenceType(arg: LambdaExpression): Option[TypeExpression] = {
        val (e, t) = arg.getTypeAnnotation(Map())
        println()
        println(e)
        println(t)
        resolveEquationSystem(new EquationSystem(e)) match {
            case Some(s) => Some(t.insertTypeExpression(s))
            case None => None
        }
    }
}
