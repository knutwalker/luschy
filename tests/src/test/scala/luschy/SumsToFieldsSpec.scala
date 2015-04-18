/*
 * Copyright 2015 Paul Horn
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package luschy

import luschy.syntax.field._
import util.{CConsFieldPrefix, Checks, FieldChecks, Fields, PrimitiveChecks}

import org.apache.lucene.document.StoredField
import org.apache.lucene.index.IndexableField
import org.scalacheck.Arbitrary
import org.specs2.matcher.{MatchResult, ThrownExpectations}
import org.specs2.{ScalaCheck, Specification}


object SumsToFieldsSpec extends Specification with ScalaCheck with ThrownExpectations with FieldChecks with PrimitiveChecks { def is = s2"""

  Support for sum types
    ADTs should
      produce a special field for the coproduct discriminant   $coproducts1
      produce nested fields from their concrete subtype        $coproducts2

   """

  sealed trait Foo
  case object FooA extends Foo
  case class FooB() extends Foo
  case class FooC(bar: String, baz: Int) extends Foo

  object Foo extends FooImplicits {
    val cases: List[String] = Fields[Foo]
    def fields(x: Foo): List[String] = Fields.of(x)
    val checks: (List[IndexableField], String, Foo) ⇒ MatchResult[_] = Checks[Foo]

    implicit def arbFoo: Arbitrary[Foo] = _arbFoo
    implicit def toFieldFoo: ToField[Foo] = _fieldsFoo
  }
  trait FooImplicits {
    import org.scalacheck.Shapeless._
    val _arbFoo: Arbitrary[Foo] = implicitly[Arbitrary[Foo]]
    val _fieldsFoo: ToField[Foo] = ToField.toFieldGeneric
  }

  private def coproducts1 = prop { (foo: Foo, name: String) ⇒
    val fs = foo.toFields(name).toList
    val f = fs.head
    checkFieldInstance[StoredField](f)
    checkFieldName(f, s"$CConsFieldPrefix$name")
    Foo.checks(fs.drop(1), name, foo)
  }

  private def coproducts2 = prop { (foo: Foo, name: String) ⇒
    val fs = foo.toFields(name).toList
    Foo.checks(fs.drop(1), name, foo)
  }
}
