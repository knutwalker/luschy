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

import org.apache.lucene.document.{DoubleField, Field, FieldType, FloatField, IntField, LongField, TextField}
import org.apache.lucene.index.IndexableField

package object util {

  final val FieldSeparator: Char = '\u001D'

  private[luschy] final val CConsFieldName =
    "__luschy_cconsinl"

  private[luschy] final val CConsFieldPrefix =
    "__luschy_ccons_"

  private[luschy] final val FieldSeparatorRe = "[\\Q\u001D\\E]"

  private final val MinimalFieldType = {
    val f = new FieldType
    f.setStored(true)
    f.freeze()
    f
  }

  private[luschy] def fieldWithNewName(field: IndexableField, name: String): IndexableField = {
    if (field.stringValue() ne null) {
      new Field(name, field.stringValue(), MinimalFieldType)
    } else if (field.binaryValue() ne null) {
      new Field(name, field.binaryValue(), MinimalFieldType)
    } else if (field.numericValue() ne null) {
      field.numericValue() match {
        case d: java.lang.Double  ⇒
          new DoubleField(name, d, Field.Store.NO)
        case f: java.lang.Float   ⇒
          new FloatField(name, f, Field.Store.NO)
        case i: java.lang.Integer ⇒
          new IntField(name, i, Field.Store.NO)
        case l: java.lang.Long    ⇒
          new LongField(name, l, Field.Store.NO)
        case x ⇒
          new IntField(name, x.intValue(), Field.Store.NO)
      }
    } else {
      new TextField(name, field.readerValue())
    }
  }
}
