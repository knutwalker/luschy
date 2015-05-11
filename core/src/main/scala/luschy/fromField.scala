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

import luschy.DecodeResults.WrongType
import util.{CConsFieldPrefix, FieldSeparatorRe, fieldWithNewName}

import org.apache.lucene.document.Document
import org.apache.lucene.index.IndexableField
import shapeless._
import shapeless.labelled._
import validation.Result
import validation.Result.symbolic._

import annotation.tailrec
import collection.JavaConverters._
import scala.util.Try

trait FromField[A] {
  def fromField(x: IndexableField, doc: Document, expectedName: String): DecodeResult[A]
}

object FromField extends FromFieldInstances {

  def apply[A](implicit A: FromField[A]): FromField[A] = A
}

trait FromFieldInstances extends FromFieldInstances1 {
  implicit val fromFieldString: FromField[String] = new FromField[String] {
    def fromField(x: IndexableField, doc: Document, expectedName: String): DecodeResult[String] = {
      if (x eq null) DecodeResults.missingField(expectedName)
      else Result.fromOption(fromString(x) orElse fromBytes(x), WrongType(expectedName, "String"))
    }

    private def fromString(x: IndexableField) =
      Option(x.stringValue())

    private def fromBytes(x: IndexableField) =
      Option(x.binaryValue()).map(_.utf8ToString())
  }

  implicit val fromFieldInd: FromField[Int] = new FromField[Int] {
    def fromField(x: IndexableField, doc: Document, expectedName: String): DecodeResult[Int] = {
      if (x eq null) DecodeResults.missingField(expectedName)
      else Result.fromOption(fromNumber(x) orElse fromText(x), WrongType(expectedName, "Int"))
    }

    private def fromNumber(x: IndexableField) =
      Option(x.numericValue()).map(_.intValue())

    private def fromText(x: IndexableField) =
      (fromString(x) orElse fromBytes(x)).flatMap(n ⇒ Try(n.toInt).toOption)

    private def fromString(x: IndexableField) =
      Option(x.stringValue())

    private def fromBytes(x: IndexableField) =
      Option(x.binaryValue()).map(_.utf8ToString())
  }
}
trait FromFieldInstances1  extends FromFieldInstances0 {

  implicit val fromFieldHNil: FromField[HNil] = new FromField[HNil] {
    def fromField(x: IndexableField, doc: Document, expectedName: String): DecodeResult[HNil] =
      Result.valid(HNil)
  }

  implicit def fromFieldHCons[K <: Symbol, V, T <: HList](implicit
    K: Witness.Aux[K],
    V: Lazy[FromField[V]],
    T: Lazy[FromField[T]])
  : FromField[FieldType[K, V] :: T] = new FromField[FieldType[K, V] :: T] {

    @tailrec
    def fromField(x: IndexableField, doc: Document, expectedName: String): DecodeResult[FieldType[K, V] :: T] = {
      val name = K.value.name
      if (x.name().startsWith(CConsFieldPrefix)) {
        val sub = getSubDoc(x, doc)
        fromField(sub.getField(name), sub, name)
      } else if (x.name() == name) {
        (V.value.fromField(x, doc, name) |@| T.value.fromField(x, doc, name)) apply (field[K](_) :: _)
      } else {
        val f = doc.getField(name)
        (V.value.fromField(f, doc, name) |@| T.value.fromField(x, doc, name)) apply (field[K](_) :: _)
      }
    }
  }

  implicit val fromFieldCNil: FromField[CNil] = new FromField[CNil] {
    def fromField(x: IndexableField, doc: Document, expectedName: String): DecodeResult[CNil] =
      DecodeResults.unexpected("fromField(CNil)")
  }

  implicit def fromFieldCCons[K <: Symbol, V, T <: Coproduct](implicit
    K: Witness.Aux[K],
    V: Lazy[FromDocument[V]],
    T: Lazy[FromField[T]])
  : FromField[FieldType[K, V] :+: T] = new FromField[FieldType[K, V] :+: T] {

    def fromField(x: IndexableField, doc: Document, expectedName: String): DecodeResult[FieldType[K, V] :+: T] = {
      if (x == null) DecodeResults.missingField(expectedName)
      else Option(x.stringValue()).filter(_ == K.value.name) match {
        case None    ⇒ T.value.fromField(x, doc, K.value.name).map(Inr(_))
        case Some(_) ⇒ V.value.fromDocument(getSubDoc(x, doc)).map(field[K](_)).map(Inl(_))
      }
    }
  }

  private def getSubDoc(x: IndexableField, doc: Document): Document = {
    val fld = x.name().replaceFirst(CConsFieldPrefix, "")
    val subDoc = new Document
    doc.getFields.asScala
      .map(f ⇒ (f, f.name().split(FieldSeparatorRe, 2).toList))
      .filter(s ⇒ s._2.head == fld && s._2.size == 2)
      .map(x ⇒ fieldWithNewName(x._1, x._2(1)))
      .foreach(subDoc.add)
    subDoc
  }
}
trait FromFieldInstances0 {
  implicit def fromFieldGeneric[T, R](implicit
    gen: LabelledGeneric.Aux[T, R],
    repr: Lazy[FromField[R]])
  : FromField[T] = new FromField[T] {
    def fromField(x: IndexableField, doc: Document, expectedName: String): DecodeResult[T] =
      repr.value.fromField(x, doc, expectedName).map(gen.from)
  }
}
