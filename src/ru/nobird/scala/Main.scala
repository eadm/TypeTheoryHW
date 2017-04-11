package ru.nobird.scala

import java.io.PrintWriter

import ru.nobird.scala.hw.{HW, HW1, HW2, HW3}

/**
  * Created by ruslandavletshin on 21/06/16.
**/

object Main {
    def main(args: Array[String]): Unit = {

//        val lambda = "(\\f.\\x.(f (f (f x))))"
//        val lambda = "(\\n.\\f.\\x.f(n f x)) (\\f.\\x.(f (f (f (f (f (f (f x))))))))" // +1
//        val lambda = "(\\n.\\f.\\x.f(n f x)) (\\f.\\x.(f x))" // +1
//        val lambda = "(\\f.\\x.(f x x))" // +1


//        val in = "((\\l0.((\\l1.((\\l2.((\\l3.((\\l4.((\\l5.((\\l6.((\\l7.((\\l8.((\\l9.((\\l10.((\\l11.((\\l12.((\\f.((f (\\x.(\\l15.(x (x l15))))) (\\x.(\\l15.(x (x (x l15))))))) (\\f.(\\x.(((l0 (\\l15.(\\l16.(\\l17.(((l1 (l10 l16)) (l12 l17)) (((l1 (l10 l17)) ((l15 (l11 l16)) (\\l18.(\\l19.(l18 l19))))) ((l15 (l11 l16)) ((l15 l16) (l11 l17))))))))) f) x))))) (\\l12.(\\f.(\\x.((l12 f) (f x))))))) (\\l11.(\\l12.(\\f.(((l11 (\\x.(\\l15.(l15 (x l12))))) (\\x.f)) (\\x.x))))))) (\\l10.((l10 (\\l11.l3)) l2)))) (l0 (\\l9.(\\l10.(\\l11.((\\l12.((\\f.(((l1 l12) f) (((l1 f) l12) ((l9 (l4 l10)) (l4 l11))))) (l8 l11))) (l8 l10)))))))) (\\l8.((l8 (\\l9.l3)) l2)))) (\\l7.(\\l8.((l8 l4) l7))))) (\\l6.(\\l7.((l6 l5) l7))))) (\\l5.(\\l6.(\\l7.((l5 l6) (l6 l7))))))) (\\l4.(\\l5.(\\l6.(((l4 (\\l7.(\\l8.(l8 (l7 l5))))) (\\l7.l6)) (\\l7.l7))))))) (\\l3.(\\l4.l4)))) (\\l2.(\\l3.l2)))) (\\l1.(\\l2.(\\l3.((l1 l2) l3)))))) (\\l0.((\\l1.(l0 (l1 l1))) (\\l1.(l0 (l1 l1))))))"
//        val lambda = "(\\n.\\f.\\x.n (\\g.\\h.h (g f)) (\\u.x) (\\u.u)) (\\f.\\x.f (f (f x)))" // -1
//        val lambda = "\\n.\\f.\\x.n (\\g.\\h.h (g f)) (\\u.x) (\\u.u)" // -1
//        val lambda = "(\\l0.((\\l1.(l0 (l1 l1))) (\\l1.(l0 (l1 l1)))))" // -1

//        val lambda = "\\f.(\\x.f(x x))(\\x.f(x x))"
//        val lambda = "(\\x.x)"

//        UntypedLambdaParser.parse(lambda) match {
//            case Success(expr, _) => test(expr.escapeBrackets())
//            case err: NoSuccess   => println(err)
//        }

//        println(new HW2().solve(lambda))

        if (args.length > 0) {
            getHWByArgs(args) match {
                case Some(hw) =>
                    val in = scala.io.Source.fromFile("task" + args(0) + ".in").mkString
                    new PrintWriter("task" + args(0) + ".out") { write(hw.solve(in)); close() }
                case None =>
                    println("Неверные аргументы")
            }
        }

//        val lambda = "let bar = \\x.(let foo = \\y.x in foo) in bar"
//
//        HMLambdaParser.parse(hm) match {
//            case Success(expr, _) => println(expr)
//            case err: NoSuccess   => println(err)
//        }

//        println(new HW3().solve(lambda))
    }

    private def getHWByArgs(args: Array[String]): Option[HW] = args(0) match {
        case "1" => Some(new HW1)
        case "2" => Some(new HW2)
        case "3" => Some(new HW3)
        case _ => None
    }
}
