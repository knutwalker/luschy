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

package luschy.util

import org.apache.lucene.index.IndexableField
import org.specs2.matcher.{AlwaysMatcher, MatchResult, MatchResultLogicalCombinators, NeverMatcher}
import shapeless._
import shapeless.labelled._

trait Checks[A] {
  def check(fs: List[IndexableField], name: String, value: A): MatchResult[_]
}
object Checks extends Checks0 {
  type Checker[A] = (List[IndexableField], String, A) => MatchResult[_]

  def resolve[A](implicit A: Checks[A]): Checks[A] = A

  def apply[A](fs: List[IndexableField], name: String, a: A)(implicit A: Checks[A]): MatchResult[_] = {
    A.check(fs, name, a)
  }

  implicit def fromFn[A](implicit fn: Checker[A]): Checks[A] = new Checks[A] {
    def check(fs: List[IndexableField], name: String, value: A): MatchResult[_] =
      fn(fs, name, value)
  }
}

trait Checks0 extends Checks1 with MatchResultLogicalCombinators {
  implicit val checksHNil: Checks[HNil] = new Checks[HNil] {
    def check(fs: List[IndexableField], name: String, value: HNil): MatchResult[_] =
      createExpectable(true).applyMatcher(AlwaysMatcher())
  }

  implicit def checksHCons[K <: Symbol, V, T <: HList](implicit
    K: Witness.Aux[K],
    V: Lazy[Checks[V]],
    T: Lazy[Checks[T]])
  : Checks[FieldType[K, V] :: T] = new Checks[FieldType[K, V] :: T] {

    def check(fs: List[IndexableField], name: String, value: FieldType[K, V] :: T): MatchResult[_] = {

      val headName = s"$name$FieldSeparator${K.value.name}"
      val checkedHead = V.value.check(List(fs.head), headName, value.head)

      if (checkedHead.isSuccess) {
        val safeTail = fs.drop(1).map(x ⇒ List(x)).flatten
        val checkedTail = T.value.check(safeTail, name, value.tail)
        checkedHead and checkedTail
      } else {
        checkedHead
      }
    }
  }

  implicit val checksCNil: Checks[CNil] = new Checks[CNil] {
    def check(fs: List[IndexableField], name: String, value: CNil): MatchResult[_] =
      createExpectable(false).applyMatcher(NeverMatcher())
  }

  implicit def checksCCons[K <: Symbol, V, T <: Coproduct](implicit
    K: Witness.Aux[K],
    V: Lazy[Checks[V]],
    T: Lazy[Checks[T]])
  : Checks[FieldType[K, V] :+: T] = new Checks[FieldType[K, V] :+: T] {

    def check(fs: List[IndexableField], name: String, value: FieldType[K, V] :+: T): MatchResult[_] = {
      value match {
        case Inl(hd) ⇒ V.value.check(fs, name, hd)
        case Inr(tl) ⇒ T.value.check(fs, name, tl)
      }
    }
  }

}

trait Checks1 {

  implicit def derive[T, R](implicit
    gen: LabelledGeneric.Aux[T, R],
    repr: Lazy[Checks[R]])
  : Checks[T] = new Checks[T] {
    def check(fs: List[IndexableField], name: String, value: T): MatchResult[_] = {
      repr.value.check(fs, name, gen.to(value))
    }
  }
}
