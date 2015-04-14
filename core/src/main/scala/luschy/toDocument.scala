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

import luschy.util.CConsFieldName

import org.apache.lucene.document.{Document, StoredField}
import shapeless._
import shapeless.labelled._

import java.nio.charset.StandardCharsets

trait ToDocument[A] {
  def toDocument(x: A): Document
}

object ToDocument extends ToDocumentInstances {

  def apply[A](implicit A: ToDocument[A]): ToDocument[A] = A
}

trait ToDocumentInstances extends ToDocumentInstances0 {

  implicit val toDocumentHNil: ToDocument[HNil] = new ToDocument[HNil] {
    def toDocument(x: HNil): Document = new Document
  }

  implicit def toDocumentHCons[K <: Symbol, V, T <: HList](implicit
    K: Witness.Aux[K],
    V: Lazy[ToField[V]],
    T: Lazy[ToDocument[T]])
  : ToDocument[FieldType[K, V] :: T] = new ToDocument[FieldType[K, V] :: T] {

    def toDocument(x: FieldType[K, V] :: T): Document = {
      val doc = T.value.toDocument(x.tail)
      V.value.toField(x.head)(K.value.name).foreach(doc.add)
      doc
    }
  }


  implicit val toDocumentCNil: ToDocument[CNil] = new ToDocument[CNil] {
    def toDocument(x: CNil): Document = new Document
  }

  implicit def toDocumentCCons[K <: Symbol, V, T <: Coproduct, N <: Nat](implicit
    K: Witness.Aux[K],
    V: Lazy[ToDocument[V]],
    T: Lazy[ToDocument[T]])
  : ToDocument[FieldType[K, V] :+: T] = new ToDocument[FieldType[K, V] :+: T] {

    def toDocument(x: FieldType[K, V] :+: T): Document = x match {
      case Inr(t) ⇒ T.value.toDocument(t)
      case Inl(v) ⇒
        val doc = V.value.toDocument(v)
        doc.add(new StoredField(CConsFieldName, K.value.name.getBytes(StandardCharsets.UTF_8)))
        doc
    }
  }
}

trait ToDocumentInstances0 {
  implicit def toDocumentGeneric[T, R](implicit
    gen: LabelledGeneric.Aux[T, R],
    repr: Lazy[ToDocument[R]])
  : ToDocument[T] = new ToDocument[T] {
    def toDocument(x: T): Document =
      repr.value.toDocument(gen.to(x))
  }
}
