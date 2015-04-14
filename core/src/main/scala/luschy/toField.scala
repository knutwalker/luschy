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

import luschy.util.{CConsFieldPrefix, FieldSeparator}

import org.apache.lucene.document.{Field, IntField, StoredField, StringField}
import org.apache.lucene.index.IndexableField
import shapeless._
import shapeless.labelled._

import collection.mutable.ListBuffer

trait ToField[A] {
  def toField(x: A)(name: String): TraversableOnce[IndexableField]
}

object ToField extends ToFieldInstances {

  def apply[A](implicit A: ToField[A]): ToField[A] = A
}

trait ToFieldInstances extends ToFieldInstances1 {
  implicit val toFieldString: ToField[String] = new ToField[String] {
    def toField(x: String)(name: String): TraversableOnce[IndexableField] =
      new StringField(name, x, Field.Store.YES) :: Nil
  }

  implicit val toFieldInd: ToField[Int] = new ToField[Int] {
    def toField(x: Int)(name: String): TraversableOnce[IndexableField] =
      new IntField(name, x, Field.Store.YES) :: Nil
  }
}
trait ToFieldInstances1  extends ToFieldInstances0 {

  implicit val toFieldHNil: ToField[HNil] = new ToField[HNil] {
    def toField(x: HNil)(name: String): TraversableOnce[IndexableField] = Nil
  }

  implicit def toFieldHCons[K <: Symbol, V, T <: HList](implicit
    K: Witness.Aux[K],
    V: Lazy[ToField[V]],
    T: Lazy[ToField[T]])
  : ToField[FieldType[K, V] :: T] = new ToField[FieldType[K, V] :: T] {

    def toField(x: FieldType[K, V] :: T)(name: String): TraversableOnce[IndexableField] = {
      val xs = new ListBuffer[IndexableField]
      xs ++= V.value.toField(x.head)(s"$name$FieldSeparator${K.value.name}")
      xs ++= T.value.toField(x.tail)(name)
      xs.result()
    }
  }

  implicit val toFieldCNil: ToField[CNil] = new ToField[CNil] {
    def toField(x: CNil)(name: String): TraversableOnce[IndexableField] = Nil
  }

  implicit def toFieldCCons[K <: Symbol, V, T <: Coproduct, N <: Nat](implicit
    K: Witness.Aux[K],
    V: Lazy[ToField[V]],
    T: Lazy[ToField[T]])
  : ToField[FieldType[K, V] :+: T] = new ToField[FieldType[K, V] :+: T] {

    def toField(x: FieldType[K, V] :+: T)(name: String): TraversableOnce[IndexableField] = x match {
      case Inr(t) ⇒ T.value.toField(t)(name)
      case Inl(v) ⇒
        val xs = new ListBuffer[IndexableField]
        xs += new StoredField(CConsFieldPrefix + name, K.value.name)
        xs ++= V.value.toField(v)(name)
        xs.result()
    }
  }
}
trait ToFieldInstances0 {
  implicit def toFieldGeneric[T, R](implicit
    gen: LabelledGeneric.Aux[T, R],
    repr: Lazy[ToField[R]])
  : ToField[T] = new ToField[T] {
    def toField(x: T)(name: String): TraversableOnce[IndexableField] =
      repr.value.toField(gen.to(x))(name)
  }
}
