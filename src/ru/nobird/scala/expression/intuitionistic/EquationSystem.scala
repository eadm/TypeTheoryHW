package ru.nobird.scala.expression.intuitionistic


/**
  * Created by ruslandavletshin on 01/03/2017.
  */
class EquationSystem(list: List[Equation]) {

    lazy val vars: Map[String, TypeExpression] = constructMapOfVariables(list)

    lazy val solvable: Boolean =
        list.forall((e) => {
            (e.getLeft.getVars.size == e.getRight.getVars.size && e.getLeft == e.getRight) ||
                (e.getLeft.isInstanceOf[TypeVariable] && !e.getRight.getVars.contains(e.getLeft.getVars.head))
        })


    def next(): Option[EquationSystem] =
        if (solvable) Some(new EquationSystem(iterate(list)))
        else None



    lazy val isSolved: Boolean =
        list.forall((e) => {
            val vl = e.getLeft.getVars
            val v = vl.head

            if (vl.size != 1 || !list.forall((e2) =>
                !e2.getRight.getVars.contains(v) &&
                    (!e2.getLeft.getVars.contains(v) || (e2.getLeft.getVars.contains(v) && e ==== e2))
            ))
                false // она не встречается в правой части
            else
                true
        })



    private def iterate(list: List[Equation]): List[Equation] = list match {
        case x :: xs =>
            val res = iterate(xs)
            if (x.isUseless)
                res
            else if (x.getRight.isInstanceOf[TypeVariable] && !x.getLeft.isInstanceOf[TypeVariable])
                x.reverse +: res
            else if (x.getRight == x.getLeft && !x.getRight.isInstanceOf[TypeVariable]) // структурное равенство
                x.expandEquations ++ res
            else {
                val vs = x.getVars
                val ff = vars.filter((p) => new Equation(new TypeVariable(p._1), p._2).toString != x.toString && vs.contains(p._1))
                x.insertTypeExpression(ff) +: res
            }
        case List() => List()
    }

    private def constructMapOfVariables(list: List[Equation]): Map[String, TypeExpression] =
        list.foldLeft(Map[String, TypeExpression]())((acc, equation) => equation.getVariable match {
            case Some(p) => acc + p
            case None => acc
        })

    override def toString: String = list.toString()
}
