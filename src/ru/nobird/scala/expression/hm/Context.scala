package ru.nobird.scala.expression.hm

import ru.nobird.scala.expression.intuitionistic.{TypeExpression, TypeVariable, UniversalQuantifier}
import ru.nobird.scala.expression.lambda.{LambdaExpression, Variable}

import scala.collection.immutable.HashMap


case class Context(private val elems: HashMap[LambdaExpression, TypeExpression]) {
    def this() {
        this(HashMap())
    }


    def replace(substitution: Substitution): Context = this

    def closure(t: TypeExpression): TypeExpression = {
        def closureConstructor(vars: List[String], t: TypeExpression): TypeExpression = vars match {
            case x :: xs => new UniversalQuantifier(TypeVariable(x), closureConstructor(xs, t))
            case List() => t
        }
        closureConstructor((t.getFreeVars -- elems.values.toSet.foldLeft(Set[String]()) {_ ++ _.getFreeVars}).toList, t)
    }

    def ++(that: Context) = Context(elems ++: that.elems)

    def -(key: Variable) = Context(elems - key)

    def +(p: (LambdaExpression, TypeExpression)) = Context(elems + p)

    def map(f: ((TypeExpression) => TypeExpression)): Context =
        Context(elems.mapValues(f) ++: HashMap[LambdaExpression, TypeExpression]())

    def get: (LambdaExpression) => Option[TypeExpression] = elems.get

    override def toString: String = elems.toString()
}
