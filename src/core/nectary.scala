/*
    Nectary, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package nectary

import fulminate.*
import anticipation.*

import scala.annotation.*
import scala.quoted.*

given Realm = realm"nectary"

@experimental
case class endpoint() extends MacroAnnotation:
  def transform(using Quotes)(tree: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
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
        abandon(msg"The `@endpoint` annotation should be applied to an object")
