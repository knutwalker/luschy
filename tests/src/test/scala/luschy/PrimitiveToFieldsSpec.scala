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

import luschy.syntax.field._
import util.FieldChecks

import org.apache.lucene.document.{IntField, StringField}
import org.specs2.matcher.ThrownExpectations
import org.specs2.{ScalaCheck, Specification}


object PrimitiveToFieldsSpec extends Specification with ScalaCheck with ThrownExpectations with FieldChecks { def is = s2"""

  Support for primitive types
    Ints should
      produce only one field                       $ints1
      be translated into an IntField               $ints2
      have the name as provided verbatim           $ints3
      have an int- and stringValue as provided     $ints4
      have no binary- or readerValue only          $ints5

    Strings should
      produce only one field                       $strings1
      be translated into a StringField             $strings2
      have the name as provided verbatim           $strings3
      have a stringValue as provided               $strings4
      provide stringValue only                     $strings5


   """

  private def ints1 = prop { (x: Int, name: String) ⇒
    val fs = x.toFields(name).toList
    checkOneField(fs)
  }

  private def ints2 = prop { (x: Int, name: String) ⇒
    val f = x.toFields(name).toList.head
    checkFieldInstance[IntField](f)
  }

  private def ints3 = prop { (x: Int, name: String) ⇒
    val f = x.toFields(name).toList.head
    checkFieldName(f, name)
  }

  private def ints4 = prop { (x: Int, name: String) ⇒
    val f = x.toFields(name).toList.head
    checkStringValue(f, x.toString)
    checkIntValue(f, x)
  }

  private def ints5 = prop { (x: Int, name: String) ⇒
    val f = x.toFields(name).toList.head
    checkNoBinaryValue(f)
    checkNoReaderValue(f)
  }

  private def strings1 = prop { (x: String, name: String) ⇒
    val fs = x.toFields(name).toList
    checkOneField(fs)
  }

  private def strings2 = prop { (x: String, name: String) ⇒
    val f = x.toFields(name).toList.head
    checkFieldInstance[StringField](f)
  }

  private def strings3 = prop { (x: String, name: String) ⇒
    val f = x.toFields(name).toList.head
    checkFieldName(f, name)
  }

  private def strings4 = prop { (x: String, name: String) ⇒
    val f = x.toFields(name).toList.head
    checkStringValue(f, x)
  }

  private def strings5 = prop { (x: String, name: String) ⇒
    val f = x.toFields(name).toList.head
    checkNoBinaryValue(f)
    checkNoNumericValue(f)
    checkNoReaderValue(f)
  }
}
