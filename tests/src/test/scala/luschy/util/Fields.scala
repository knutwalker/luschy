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

import org.specs2.matcher.MatchResultLogicalCombinators
import shapeless._
import shapeless.labelled._


trait Fields[A] {
  def apply(): List[String]
  def apply(x: A): List[String]
}
object Fields extends Fields0 {

  def resolve[A](implicit A: Fields[A]): Fields[A] = A

  def apply[A](implicit A: Fields[A]): List[String] =
    A()

  def of[A](x: A)(implicit A: Fields[A]): List[String] =
    A(x)
}

trait Fields0 extends Fields1 with MatchResultLogicalCombinators {
  implicit val fieldsHNil: Fields[HNil] = new Fields[HNil] {
    def apply(): List[String] = List()
    def apply(x: HNil): List[String] = List()
  }

  implicit def fieldsHCons[K <: Symbol, V, T <: HList](implicit
    K: Witness.Aux[K],
    T: Lazy[Fields[T]])
  : Fields[FieldType[K, V] :: T] = new Fields[FieldType[K, V] :: T] {
    def apply(): List[String] = K.value.name :: T.value.apply()
    def apply(x: FieldType[K, V] :: T): List[String] = apply()
  }

  implicit val fieldsCNil: Fields[CNil] = new Fields[CNil] {
    def apply(): List[String] = List()
    def apply(x: CNil): List[String] = List()
  }

  implicit def fieldsCCons[K <: Symbol, V, T <: Coproduct](implicit
    K: Witness.Aux[K],
    V: Lazy[Fields[V]],
    T: Lazy[Fields[T]])
  : Fields[FieldType[K, V] :+: T] = new Fields[FieldType[K, V] :+: T] {
    def apply(): List[String] = {
      K.value.name :: T.value.apply()

    }
    def apply(x: FieldType[K, V] :+: T): List[String] = x match {
      case Inr(tl) ⇒ T.value.apply(tl)
      case Inl(hd) ⇒
        val k = K.value.name
        V.value.apply(hd).map(n ⇒ s"$k$FieldSeparator$n")
    }
  }
}

trait Fields1 {

  implicit def derive[T, R](implicit
    gen: LabelledGeneric.Aux[T, R],
    repr: Lazy[Fields[R]])
  : Fields[T] = new Fields[T] {
    def apply(): List[String] =
      repr.value.apply()
    def apply(x: T): List[String] =
      repr.value.apply(gen.to(x))
  }
}
