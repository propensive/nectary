package nectary

import fulminate.*

import scala.annotation.*
import scala.quoted.*

@experimental
case class endpoint() extends MacroAnnotation:
  def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    import quotes.reflect.*
    tree match
      case classDef@ClassDef(name, ctr, parents, self, body) =>
        println("body = "+body)
        println("name = "+name)
        println("ctr = "+ctr)
        println("parents = "+parents)
        println("self = "+self)
        val intInfo = Symbol.requiredMethod("java.lang.Object.hashCode").info
        println(Symbol.requiredMethod("java.lang.Object.hashCode").info.show)
        println(MethodType(List("z"))(_ => List(TypeRepr.of[String]), _ => TypeRepr.of[Int]).show)
        val sym = Symbol.newMethod(classDef.symbol, "other", intInfo, Flags.EmptyFlags, Symbol.noSymbol)

        val newDef = DefDef(sym, _ => Some(Literal(IntConstant(42))))
        val newObj = ClassDef.copy(classDef)(name, ctr, parents, self, newDef :: body)
        List(newObj)
      case _ =>
        fail(msg"The `@endpoint` annotation should be applied to an object")
