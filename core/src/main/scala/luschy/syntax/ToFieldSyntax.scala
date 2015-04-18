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

package luschy.syntax

import luschy.ToField

import org.apache.lucene.index.IndexableField

trait ToFieldSyntax {

  implicit final class ToFieldOps[A](val x: A) {
    def toFields(name: String)(implicit A: ToField[A]): TraversableOnce[IndexableField] =
      A.toField(x)(name)

    def toFields(name: Symbol)(implicit A: ToField[A]): TraversableOnce[IndexableField] =
      A.toField(x)(name.name)
  }

  implicit final class ToNamedFieldOps(val name: String) {
    def asFieldFor[A](x: A)(implicit A: ToField[A]): TraversableOnce[IndexableField] =
      A.toField(x)(name)
  }
}

object ToFieldSyntax extends ToFieldSyntax
