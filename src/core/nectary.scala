/*
    Nectary, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import jacinta.*
import merino.*
import prepositional.*
import turbulence.*
import vacuous.*

import scala.annotation.*
import scala.quoted.*

given Realm = realm"nectary"

object OpenApiError:
  enum Reason:
    case Json, BadRef

  given Reason is Communicable =
    case Reason.Json   => m"the JSON content could not be parsed"
    case Reason.BadRef => m"the field was not a reference"

import OpenApiError.Reason

case class OpenApiError(reason: OpenApiError.Reason)
extends Error(m"Could not read OpenAPI spec becaus $reason")

object OpenApi:
  def parse[SourceType: Readable by Bytes](source: SourceType): OpenApi raises OpenApiError =
    tend:
      case JsonParseError(_, _, _) => OpenApiError(Reason.Json)
      case JsonError(_)            => OpenApiError(Reason.Json)

    . within(Json.parse(source).as[OpenApi])

  case class Info(title: Text, description: Text, version: Text)
  case class Server(url: Text, description: Text)
  case class Spec(get: Optional[Interaction], post: Optional[Interaction])
  case class Parameter(name: Text, in: Text, required: Boolean, schema: Json)
  case class Interaction(summary: Text, parameters: List[Parameter], responses: Map[Text, Json])
  case class Components()

case class OpenApi(openapi: Text, info: OpenApi.Info, servers: List[OpenApi.Server], paths: Map[Text, OpenApi.Spec], components: OpenApi.Components, security: List[Json], tags: List[Json])

object Ref:
  given (using Tactic[OpenApiError], Tactic[JsonError]) => Ref is Decodable in Json =
    summon[Map[Text, Json] is Decodable in Json].map: map =>
      if map.size != 1 || !map.contains(t"$$ref") then abort(OpenApiError(Reason.BadRef)) else
        map(t"$$ref").as[Text].cut(t"/") match
          case t"#" :: more => Ref(more)
          case _            => abort(OpenApiError(Reason.BadRef))

case class Ref(path: List[Text])

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
        halt(m"The `@endpoint` annotation should be applied to an object")
