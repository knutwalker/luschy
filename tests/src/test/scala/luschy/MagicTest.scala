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

import org.apache.lucene.document.{Document, Field, IntField, StringField}
import org.apache.lucene.index._
import org.specs2.mutable.Specification
import org.specs2.scalaz.ScalazMatchers


object MagicTest extends Specification with ScalazMatchers with LuceneContext {

  sealed trait Animal
  case class Dog(name: String, bones: Int) extends Animal
  case class Cat(name: String, fishes: Int) extends Animal

  case class Person(name: String, age: Int, pet: Animal)

  val bernd = Person("Bernd", 42, Cat("Tigger", 21))
  val ralle = Person("Ralle", 13, Dog("Rolf", 37))

  "using the default API" >> { iw: IndexWriter ⇒

    val doc1 = new Document
    doc1.add(new StringField("name", bernd.name, Field.Store.YES))
    doc1.add(new IntField("age", bernd.age, Field.Store.YES))
    bernd.pet match {
      case Dog(name, bones) ⇒
        doc1.add(new StringField("pet.name", name, Field.Store.YES))
        doc1.add(new IntField("pet.bones", bones, Field.Store.YES))
      case Cat(name, fishes) ⇒
        doc1.add(new StringField("pet.name", name, Field.Store.YES))
        doc1.add(new IntField("pet.fishes", fishes, Field.Store.YES))
    }
    iw.addDocument(doc1)

    val doc2 = new Document
    doc2.add(new StringField("name", ralle.name, Field.Store.YES))
    doc2.add(new IntField("age", ralle.age, Field.Store.YES))
    ralle.pet match {
      case Dog(name, bones) ⇒
        doc2.add(new StringField("pet.name", name, Field.Store.YES))
        doc2.add(new IntField("pet.bones", bones, Field.Store.YES))
      case Cat(name, fishes) ⇒
        doc2.add(new StringField("pet.name", name, Field.Store.YES))
        doc2.add(new IntField("pet.fishes", fishes, Field.Store.YES))
    }
    iw.addDocument(doc2)

    val result = search(iw)("Bernd", "name")

    result should have size 1
    val hit: Document = result.head

    hit should equal (doc1)
    (Cat(
      hit.getField("pet.name").stringValue(),
      hit.getField("pet.fishes").stringValue().toInt
    ): Animal) should be equalTo bernd.pet
    Cat(
      hit.getField("pet.name").stringValue(),
      hit.getField("pet.fishes").stringValue().toInt
    ) should beEqualTo (bernd.pet)
    hit.getField("pet.fishes").stringValue().toInt should be equalTo bernd.pet.asInstanceOf[Cat].fishes
  }

  "Woah! serious magic!!1! pew pew pew" >> { implicit iw: IndexWriter ⇒
    import syntax.all._

    bernd.addToIndex
    ralle.addToIndex

    val result = search(iw)("Bernd", "name")

    result should have size 1
    val hit: Document = result.head

    hit.as[Person] should be equalTo bernd
    (hit >> 'pet).as[Animal] should be equalTo bernd.pet
    hit.field('pet)[Animal] should be equalTo hit.as[Person].pet
    hit.field('pet).as[Cat] should beEqualTo (bernd.pet)
    (hit >> 'pet >> 'fishes)[Int] should be equalTo bernd.pet.asInstanceOf[Cat].fishes
  }
}
