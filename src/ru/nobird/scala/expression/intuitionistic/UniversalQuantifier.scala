package ru.nobird.scala.expression.intuitionistic

/**
  * Created by ruslandavletshin on 09/04/2017.
  */
class UniversalQuantifier(private val v: TypeVariable, private val e: TypeExpression)
    extends TypeExpression("@" + v + "." + e) {

    override def ==(that: TypeExpression): Boolean = that match {
        case u: UniversalQuantifier => u.v == v && u.e == v
        case _ => false
    }

    override def insertTypeExpression(vars: Map[String, TypeExpression]): TypeExpression =
        new UniversalQuantifier(v, e.insertTypeExpression(vars - v.toString))

    override def getVars: Set[String] = e.getVars
    override def getFreeVars: Set[String] = e.getFreeVars - v.toString

    override def unwrap(): TypeExpression = e match {
        case e1: UniversalQuantifier => e1.unwrap()
        case _ => e
    }
}
