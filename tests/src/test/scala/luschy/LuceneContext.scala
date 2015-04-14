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

import scalaz._
import Scalaz._

import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.document.Document
import org.apache.lucene.index.{DirectoryReader, IndexWriter, IndexWriterConfig, Term}
import org.apache.lucene.search.{IndexSearcher, TermQuery}
import org.apache.lucene.store.RAMDirectory
import org.specs2.execute.{AsResult, Result}
import org.specs2.specification.ForEach

import collection.JavaConverters._

trait LuceneContext extends ForEach[IndexWriter] {

  protected def foreach[R: AsResult](f: IndexWriter ⇒ R): Result = {
    val dir = new RAMDirectory()
    val iwc = new IndexWriterConfig(new StandardAnalyzer)
    val iw = new IndexWriter(dir, iwc)

    try AsResult(f(iw))
    finally {
      iw.close()
      dir.close()
    }
  }

  def search(iw: IndexWriter)(q: String, f: String): List[Document] = {
    iw.commit()
    val dir = iw.getDirectory
    val ir = DirectoryReader.open(dir)
    val is = new IndexSearcher(ir)
    val docs = is.search(new TermQuery(new Term(f, q)), 10)
    val result = docs.scoreDocs.toList.map(d ⇒ is.doc(d.doc))
    ir.close()
    result
  }

  implicit val equalDocument: Equal[Document] = Equal.equalBy { (d: Document) ⇒
    d.getFields.asScala.map(f ⇒ {
      val value = Option(f.stringValue()) orElse Option(f.numericValue()).map(_.toString) getOrElse ""
      (f.name(), value)
    }).toList
  }
  implicit val showDocument: Show[Document] = Show.showFromToString
}
