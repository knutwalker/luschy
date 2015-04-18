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
import util.{Checks, FieldChecks, FieldSeparator, Fields, PrimitiveChecks}

import org.apache.lucene.index.IndexableField
import org.scalacheck.Arbitrary
import org.specs2.matcher.{MatchResult, ThrownExpectations}
import org.specs2.{ScalaCheck, Specification}
import shapeless._
import shapeless.syntax.singleton._

object ProductsToFieldsSpec extends Specification with ScalaCheck with ThrownExpectations with FieldChecks with PrimitiveChecks { def is = s2"""

  Support for product types
    case classes should
      produce a combined name of the base and each field name       $cases1
      produce fields that are otherwise the same as their fields    $cases2

    records should
      produce a combined name of the base and each field name       $records1
      produce fields that are otherwise the same as their fields    $records2

    tuples should
      produce a combined name of the base and each field name       $tuples1
      produce fields that are otherwise the same as their fields    $tuples2

   """

  case class Foo(bar: String, baz: Int)
  object Foo extends ArbFoo {
    val fields: List[String] = Fields[Foo]
    val checks: (List[IndexableField], String, Foo) ⇒ MatchResult[_] = Checks[Foo]

    implicit val arbFoo: Arbitrary[Foo] = _arbFoo
  }
  trait ArbFoo {
    import org.scalacheck.Shapeless._
    val _arbFoo = implicitly[Arbitrary[Foo]]
  }

  private def cases1 = prop { (foo: Foo, name: String) ⇒
    val fs = foo.toFields(name).toList
    fs.zip(Foo.fields) map { case (f, n) ⇒
      checkFieldName(f, s"$name$FieldSeparator$n")
    }
  }

  private def cases2 = prop { (foo: Foo, name: String) ⇒
    val fs = foo.toFields(name).toList
    Checks(fs, name, foo)
  }

  private def records1 = prop { (x: String, y: Int, name: String) ⇒
    val fs = (('bar ->> x) :: ('baz ->> y) :: HNil).toFields(name).toList
    fs.zip(List("bar", "baz")) map { case (f, n) ⇒
      checkFieldName(f, s"$name$FieldSeparator$n")
    }
  }

  private def records2 = prop { (x: String, y: Int, name: String) ⇒
    val foo = ('bar ->> x) :: ('baz ->> y) :: HNil
    val fs = foo.toFields(name).toList
    Checks(fs, name, foo)
  }

  private def tuples1 = prop { (foo: (String, Int), name: String) ⇒
    val fs = foo.toFields(name).toList
    fs.zip(List("_1", "_2")) map { case (f, n) ⇒
      checkFieldName(f, s"$name$FieldSeparator$n")
    }
  }

  private def tuples2 = prop { (foo: (String, Int), name: String) ⇒
    val fs = foo.toFields(name).toList
    Checks(fs, name, foo)
  }
}
