package ru.nobird.scala.expression.hm

import ru.nobird.scala.expression.intuitionistic.{TypeExpression, TypeVariable, UniversalQuantifier}


class Context(private val list: Vector[TypedLambdaExpression]) {
    def this() {
        this(List())
    }


    def replace(substitution: Substitution): Context = this

    def closure(t: TypeExpression): TypeExpression = {
        def closureConstructor(vars: List[String], t: TypeExpression): TypeExpression = vars match {
            case x :: xs => new UniversalQuantifier(new TypeVariable(x), closureConstructor(xs, t))
            case List() => t
        }
        closureConstructor((t.getFreeVars -- list.foldLeft(Set[String]()) {_ ++ _.getType.getFreeVars}).toList, t)
    }

    def ++(that: Context) = new Context(list ++ that.list)
}
