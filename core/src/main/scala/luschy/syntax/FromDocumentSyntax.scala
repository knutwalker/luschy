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

import util._

import org.apache.lucene.document.Document
import org.apache.lucene.index.IndexableField

trait FromDocumentSyntax {

  implicit final class FromDocumentOps(val doc: Document) {
    def as[A](implicit A: FromDocument[A]): A =
      A.fromDocument(doc)

    def field(name: Symbol): SelectedField = {
      // TODO: cannot witness singleton type of K <: Symbol(name), how to do???
      // TODO: So that we can reuse fromDocumentHCons, maybe?
      SelectedField(name.name, doc)
    }

    def field[A](names: String*): SelectedField = {
      val fld = names.mkString(FieldSeparator.toString)
      SelectedField(fld, doc)
    }
  }

  final class SelectedField(field: IndexableField, doc: Document) {
    def as[A](implicit A: FromField[A]): A = {
      A.fromField(field, doc)
    }
  }
  object SelectedField {
    def apply(name: String, doc: Document): SelectedField = {
      val field = Option(doc.getField(name))
        .getOrElse(doc.getField(CConsFieldPrefix + name))
      new SelectedField(field, doc)
    }
  }
}

object FromDocumentSyntax extends FromDocumentSyntax
