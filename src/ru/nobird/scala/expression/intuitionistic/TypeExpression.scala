package ru.nobird.scala.expression.intuitionistic

import ru.nobird.scala.expression.Expression

/**
  * Created by ruslandavletshin on 01/03/2017.
  */
abstract class TypeExpression(s: String) extends Expression(s) {

    def ==(that: TypeExpression): Boolean
    def ===(that: TypeExpression): Boolean = this == that
    def ====(that: Expression): Boolean = that.toString == this.toString

    def insertTypeExpression(vars: Map[String, TypeExpression]): TypeExpression

    def getVars: Set[String]

    def getFreeVars: Set[String]
}
