package ru.nobird.scala.expression.intuitionistic
/**
  * Created by ruslandavletshin on 01/03/2017.
  */
case class TypeVariable(x: String) extends TypeExpression(x) {

    def getVar: String = x

    override def ==(that: TypeExpression): Boolean = that.isInstanceOf[TypeVariable]

    override def ===(that: TypeExpression): Boolean = that match {
        case v:TypeVariable => v.getVar == x
        case _ => false
    }



    override def insertTypeExpression(vars: Map[String, TypeExpression]): TypeExpression = vars.get(x) match {
        case Some(xx) => xx
        case _ => this
    }

    override def getVars: Set[String] = Set(x)
    override def getFreeVars: Set[String] = getVars
}
