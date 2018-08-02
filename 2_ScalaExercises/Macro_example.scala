// macro example

import language.experimental.macros
import reflect.macros.Context

object DebugMacros extends App{
 def hello(): Unit = macro hello_impl
 def hello_impl(c: Context)(): c.Expr[Unit] = {
  import c.universe._
  reify { println("First example") }
 }
}
