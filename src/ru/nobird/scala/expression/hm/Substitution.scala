package ru.nobird.scala.expression.hm

class Substitution {

    def apply(context: Context): Context = context.replace(this)

//    def apply(substitution: Substitution): Substitution = new Substitution(substitution)
}
