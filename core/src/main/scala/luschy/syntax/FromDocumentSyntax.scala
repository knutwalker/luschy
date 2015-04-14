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
package syntax

import util.FieldSeparator

import org.apache.lucene.document.Document
import shapeless._
import shapeless.tag.@@

trait FromDocumentSyntax {

  implicit final class FromDocumentOps(val doc: Document) {
    def as[A](implicit A: FromDocument[A]): A =
      A.fromDocument(doc)

    def field[K <: Symbol](implicit name: Witness.Aux[K]): SelectedField[K] =
      new SelectedField[K](doc, name)

    def >>[K <: Symbol](implicit name: Witness.Aux[K]): SelectedField[K] =
      field(name)
  }

  final class SelectedField[K <: Symbol](doc: Document, K: Witness.Aux[K]) {
    def as[A](implicit A: Lazy[FromField[A]]): A = {
      val fromDoc = FromDocument.fromDocumentHCons[K, A, HNil](K, A, Lazy(FromDocument.fromDocumentHNil))
      fromDoc.fromDocument(doc).head
    }

    def apply[A](implicit A: Lazy[FromField[A]]): A =
      as[A]

    //noinspection TypeAnnotation
    def field[L <: Symbol](implicit L: Witness.Aux[L]) = {
      val fuse = K.value.name + FieldSeparator + L.value.name
      val F = new Witness {
        type T = Symbol @@ fuse.type
        val value: Symbol @@ fuse.type =
          tag[fuse.type](Symbol(fuse))
      }
      new SelectedField[F.T](doc, F)
    }

    //noinspection TypeAnnotation
    def >>[L <: Symbol](implicit L: Witness.Aux[L]) =
      field(L)
  }
}

object FromDocumentSyntax extends FromDocumentSyntax
