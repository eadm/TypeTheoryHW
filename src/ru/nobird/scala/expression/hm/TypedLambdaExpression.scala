package ru.nobird.scala.expression.hm

import ru.nobird.scala.expression.intuitionistic.TypeExpression
import ru.nobird.scala.expression.lambda.LambdaExpression

/**
  * Created by ruslandavletshin on 09/04/2017.
  */
class TypedLambdaExpression(expression: LambdaExpression, t: TypeExpression) {

    def getType: TypeExpression = t


}
