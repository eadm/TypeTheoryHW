package ru.nobird.scala.expression.hm

import ru.nobird.scala.expression.intuitionistic.TypeExpression

case class Substitution(map: Map[String, TypeExpression]) {

    def this() = this(Map())

    def apply(context: Context): Context = context.map((t) => this(t))
    def apply(t: TypeExpression): TypeExpression = t.insertTypeExpression(map)

    def apply(substitution: Substitution): Substitution =
        Substitution(substitution.map.map((p) => (p._1, this(p._2))) ++ map)

    def get: (String) => Option[TypeExpression] = map.get

    def ++(that: Substitution): Substitution = Substitution(map ++ that.map)

    def ++!(that: Substitution): Substitution =
        if (map.forall((p) => that.get(p._1) match {
            case Some(tt) => p._2 == tt
            case None => true
        }))
            this ++ that
        else
            throw new TypeInferenceException("Can't resolve substitution")

    def +(p: (String, TypeExpression)): Substitution = Substitution(map + p)
}
