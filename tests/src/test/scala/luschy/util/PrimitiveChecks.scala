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

import org.apache.lucene.document.{IntField, StringField}
import org.apache.lucene.index.IndexableField
import org.specs2.matcher.MatchResult

trait PrimitiveChecks { this: FieldChecks ⇒

  implicit def allIntChecks(fs: List[IndexableField], name: String, value: Int): MatchResult[_] = {
    checkOneField(fs)
    val f = fs.head
    checkFieldInstance[IntField](f)
    checkFieldName(f, name)
    checkStringValue(f, value.toString)
    checkIntValue(f, value)
    checkNoBinaryValue(f)
    checkNoReaderValue(f)
  }

  implicit def allStringChecks(fs: List[IndexableField], name: String, value: String): MatchResult[_] = {
    checkOneField(fs)
    val f = fs.head
    checkFieldInstance[StringField](f)
    checkFieldName(f, name)
    checkStringValue(f, value)
    checkNoBinaryValue(f)
    checkNoNumericValue(f)
    checkNoReaderValue(f)
  }
}
