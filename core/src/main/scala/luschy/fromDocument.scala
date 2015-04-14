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

import luschy.util.{CConsFieldName, CConsFieldPrefix}

import org.apache.lucene.document.Document
import shapeless._
import shapeless.labelled._

trait FromDocument[A] {
  def fromDocument(x: Document): DecodeResult[A]
}

object FromDocument extends FromDocumentInstances {

  def apply[A](implicit A: FromDocument[A]): FromDocument[A] = A
}

trait FromDocumentInstances extends FromDocumentInstances0 {

  implicit val fromDocumentHNil: FromDocument[HNil] = new FromDocument[HNil] {
    def fromDocument(x: Document): DecodeResult[HNil] = DecodeResult.valid(HNil)
  }

  implicit def fromDocumentHCons[K <: Symbol, V, T <: HList](implicit
    K: Witness.Aux[K],
    V: Lazy[FromField[V]],
    T: Lazy[FromDocument[T]])
  : FromDocument[FieldType[K, V] :: T] = new FromDocument[FieldType[K, V] :: T] {

    def fromDocument(x: Document): DecodeResult[FieldType[K, V] :: T] = {
      val headField = Option(x.getField(K.value.name))
        .getOrElse(x.getField(CConsFieldPrefix + K.value.name))
      (V.value.fromField(headField, x, K.value.name) |@| T.value.fromDocument(x))(field[K](_) :: _)
    }
  }

  implicit val fromDocumentCNil: FromDocument[CNil] = new FromDocument[CNil] {
    def fromDocument(x: Document): DecodeResult[CNil] =
      DecodeResult.unexpected("fromDocument(CNil)")
  }

  implicit def fromDocumentCCons[K <: Symbol, V, T <: Coproduct, N <: Nat](implicit
    K: Witness.Aux[K],
    V: Lazy[FromDocument[V]],
    T: Lazy[FromDocument[T]])
  : FromDocument[FieldType[K, V] :+: T] = new FromDocument[FieldType[K, V] :+: T] {

    def fromDocument(x: Document): DecodeResult[FieldType[K, V] :+: T] = {
      Option(x.getField(CConsFieldName))
        .flatMap(f ⇒ Option(f.binaryValue()))
        .map(_.utf8ToString())
        .filter(_ == K.value.name) match {
        case Some(_) ⇒ V.value.fromDocument(x).map(field[K](_)).map(Inl(_))
        case None ⇒ T.value.fromDocument(x).map(Inr(_))
      }
    }
  }
}

trait FromDocumentInstances0 {
  implicit def fromDocumentGeneric[T, R](implicit
    gen: LabelledGeneric.Aux[T, R],
    repr: Lazy[FromDocument[R]])
  : FromDocument[T] = new FromDocument[T] {
    def fromDocument(x: Document): DecodeResult[T] =
      repr.value.fromDocument(x).map(gen.from)
  }
}
