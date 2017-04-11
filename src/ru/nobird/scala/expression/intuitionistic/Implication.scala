package ru.nobird.scala.expression.intuitionistic


/**
  * Created by ruslandavletshin on 01/03/2017.
  */
case class Implication(left: TypeExpression, right: TypeExpression) extends TypeExpression("(" + left + " -> " + right + ")") {

    override def ==(that: TypeExpression): Boolean = that match {
        case impl: Implication => left == impl.getLeft && right == impl.getRight
        case _ => false
    }

    override def insertTypeExpression(vars: Map[String, TypeExpression]): TypeExpression =
        new Implication(left.insertTypeExpression(vars), right.insertTypeExpression(vars))

    def getLeft: TypeExpression = left
    def getRight: TypeExpression = right

    override def getVars: Set[String] = left.getVars ++ right.getVars
    override def getFreeVars: Set[String] = left.getFreeVars ++ right.getFreeVars
}
