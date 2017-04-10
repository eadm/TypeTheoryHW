package ru.nobird.scala.expression.intuitionistic

/**
  * Created by ruslandavletshin on 01/03/2017.
  */
class Equation(left: TypeExpression, right: TypeExpression) extends TypeExpression(left + " = " + right) {

    def isUseless:Boolean = left ==== right

    override def ==(that: TypeExpression): Boolean = that match {
        case eq: Equation => left == eq.getLeft && right == eq.getRight
        case _ => false
    }

    override def insertTypeExpression(vars: Map[String, TypeExpression]): Equation =
        new Equation(left.insertTypeExpression(vars), right.insertTypeExpression(vars))

    lazy val reverse: Equation = new Equation(right, left)

    lazy val getVariable: Option[(String, TypeExpression)] = left match {
        case v: TypeVariable => Some(v.toString, right)
        case _ => None
    }

    def getLeft: TypeExpression = left
    def getRight: TypeExpression = right

    def expandEquations: List[Equation] = expandEquations(left, right)

    private def expandEquations(left: TypeExpression, right: TypeExpression): List[Equation] = (left, right) match {
        case (l: TypeVariable, r: TypeVariable) => List(new Equation(l, r))
        case (l: Implication, r: Implication) => expandEquations(l.getLeft, r.getLeft) ++ expandEquations(l.getRight, r.getRight)
    }

    override def getVars: Set[String] = left.getVars ++ right.getVars
    override def getFreeVars: Set[String] = left.getFreeVars ++ right.getFreeVars
}
