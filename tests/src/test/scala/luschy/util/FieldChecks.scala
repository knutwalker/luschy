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
import org.specs2.matcher.{ShouldMatchers, ThrownExpectations}

import reflect.ClassTag

trait FieldChecks { this: ShouldMatchers with ThrownExpectations ⇒

  protected def checkOneField(fs: List[IndexableField]) =
    fs should have size 1

  protected def checkFieldInstance[T <: IndexableField : ClassTag](f: IndexableField) =
    f should beAnInstanceOf[T]

  protected def checkFieldName(f: IndexableField, name: String) =
    f.name() should be_===(name)

  protected def checkStringValue(f: IndexableField, x: String) =
    f.stringValue() should be_===(x.toString)

  protected def checkIntValue(f: IndexableField, x: Int) =
    f.numericValue().intValue() should be_===(x)

  protected def checkNoBinaryValue(f: IndexableField) =
    f.binaryValue() should beNull

  protected def checkNoReaderValue(f: IndexableField) =
    f.readerValue() should beNull

  protected def checkNoNumericValue(f: IndexableField) =
    f.numericValue() should beNull

}
