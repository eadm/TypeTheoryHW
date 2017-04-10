package ru.nobird.scala.expression.lambda

import ru.nobird.scala.expression.Expression
import ru.nobird.scala.expression.intuitionistic.{Equation, EquationSystem, TypeExpression}


/**
  * Created by ruslandavletshin on 21/06/16.
  */
abstract class LambdaExpression(s: String) extends Expression(s) {
    def betaReduction(vars: Map[String, LambdaExpression]): LambdaExpression

    val isInNormalForm: Boolean
    val isBetaRedex: Boolean

    def getTypeAnnotation(cache: Map[String, TypeExpression]): (List[Equation], TypeExpression)

    def escapeBrackets(): LambdaExpression
}

object LambdaExpression {
    def normalise(arg: LambdaExpression): LambdaExpression = {
        var e = arg
        var cc = 0
        while (!e.isInNormalForm) {
            e = e.betaReduction(Map())
            cc+=1
//            println()
//            println()
//            println(e)
            if (cc % 1000 == 0) println(cc)
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

//    def getNextTypeVar: String = "a_" + getNext
    def getNextTypeVar: String = alph(getNext)


    def resolveEquationSystem(s: EquationSystem): Map[String, TypeExpression] = {

        var r = s
        println(r)

        while (!r.isSolved) {
            r = r.next().get
            println(r)
        }

        r.vars
    }

    def inferenceType(arg: LambdaExpression): Unit = {
        val tmp = arg.getTypeAnnotation(Map())

        println("\nSystem: ")
        println(tmp._1)

        println("\nTau: ")
        println(tmp._2)

        println()

        val S = resolveEquationSystem(new EquationSystem(tmp._1))
        println(S)

        println(tmp._2.insertTypeExpression(S))
    }
}
