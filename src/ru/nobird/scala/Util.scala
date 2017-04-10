package ru.nobird.scala

/**
  * Created by ruslandavletshin on 21/06/16.
  */
object Util {
    def removeWhitespaces(s: String): String = s.filterNot(c => c.isWhitespace)
}
