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

import validation.NonEmptyVector

final case class SimpleSearchRequest(
  query: SearchString,
  size: Size, // page
  skip: Skip, // per page
  defaultField: Field,
  sort: List[Sort],
  filter: List[SimpleFilter],
  token: Option[Long],
  requestStart: Option[Long])

final case class SearchRequest(
  query: SearchString,
  size: Size, // page
  skip: Skip, // per page
  defaultField: Field,
  sort: List[Sort],
  preFilter: List[Filter], // TODO: filterScope (query, global, facets, etc)
  filter: List[Filter],
  facets: List[Facet],
  token: Option[Long],
  requestStart: Option[Long])

case class Sort(
  field: Field,
  order: SortOrder)

final case class SimpleFilter(
  name: Name,
  values: List[String])

final case class Filter(
  name: Name,
  values: List[String] = Nil,
  filterType: FilterType = FilterType.DrillSideways,
  facetType: FacetType = FacetType.Term)

final case class Facet(
  name: Name,
  field: Field,
  `type`: FacetType = FacetType.Term)


sealed trait SortOrder
object SortOrder {
  case object Desc extends SortOrder
  case object Asc extends SortOrder

  def reverse(s: SortOrder): SortOrder = s match {
    case Desc ⇒ Asc
    case Asc  ⇒ Desc
  }
}

sealed trait FilterType
object FilterType {
  case object DrillDown extends FilterType
  case object DrillSideways extends FilterType
  case object MultiSelect extends FilterType // TODO: == DrillSideways ?
  case object None extends FilterType
}

sealed trait FilterScope
object FilterScope {
  case object Global extends FilterScope
  case object Query extends FilterScope
  case object Facet extends FilterScope
}

sealed trait FacetType
object FacetType {
  case object Term extends FacetType
  case object Union extends FacetType // TODO: better name
  case object Range extends FacetType
  case object Exclude extends FacetType
  case object Missing extends FacetType
  case object Other extends FacetType
}


case class SearchResult[A](
  request: SearchRequest,
  total: Long = 0,
  maxScore: Score = Score(-1),
  took: Long = -1,
  token: Long,
  docs: List[ResultDoc[A]] = List(),
  highlights: Option[String] = None,
  facets: List[FacetResult] = List(),
  terms: Option[String] = None) {

  def map[B](f: A ⇒ B): SearchResult[B] =
    copy(docs = docs.map(_.map(f)))

  def mapcat[B](f: A ⇒ List[B]): SearchResult[B] =
    copy(docs = docs.flatMap(_.mapseq(f)))
}

case class ResultDoc[A](
  id: String,
  score: Score,
  fields: Map[String, String],
  sort: List[String],
  filter: List[String],
  payload: Option[Array[Byte]],
  payload2: Option[A]) {

  def map[B](f: A ⇒ B): ResultDoc[B] =
    copy(payload2 = payload2.map(f))

  def mapseq[B](f: A ⇒ List[B]): List[ResultDoc[B]] =
    payload2.toList.flatMap(f).map(x ⇒ copy(payload2 = Some(x)))
}

case class FacetResult(
  name: Name,
  `type`: FacetType,
  size: Size,
  total: Long,
  missing: Long,
  other: Long,
  entries: List[SearchFacetEntry])

case class SearchFacetEntry(
  name: Name,
  values: NonEmptyVector[String],
  count: Int)

case class AnalyzeRequest(
  names: List[Name],
  text: String,
  field: Field,
  includes: List[AnalyzerAttribute],
  tokensOnly: Boolean = false)

case object AvailableAnalyzers

sealed trait AnalyzeResponse {
  def analyzer: Name
  def className: String
}
object AnalyzeResponse {
  case class Response(
    tokens: List[Map[String, Option[Any]]],
    analyzer: Name,
    className: String)
    extends AnalyzeResponse

  case class TokensResponse(
    tokens: List[String],
    analyzer: Name,
    className: String)
    extends AnalyzeResponse
}

case class AnalyzerAttributes(
  withToken: Boolean = false,
  withOffsets: Boolean = false,
  withPosition: Boolean = false)

sealed trait AnalyzerAttribute
object AnalyzerAttribute {
  case object Tokens extends AnalyzerAttribute
  case object Offsets extends AnalyzerAttribute
  case object Positions extends AnalyzerAttribute
  case object Lengths extends AnalyzerAttribute
  case object Payload extends AnalyzerAttribute
  case object Type extends AnalyzerAttribute
  case object Flags extends AnalyzerAttribute
  case object Keyword extends AnalyzerAttribute

  val available =
    Set(Tokens, Offsets, Positions, Lengths, Payload, Type, Flags, Keyword)
}


final case class Name(x: String) extends AnyVal
final case class Field(x: String) extends AnyVal
final case class SearchString(x: String) extends AnyVal
final case class Skip(x: Int) extends AnyVal
final case class Score(x: Float) extends AnyVal
final case class Size(x: Int) extends AnyVal {
  def min(y: Int): Size = Size(x min y)
}
object Skip {
  val zero = Skip(0)
}
object Size {
  val zero = Size(0)
  val ten = Size(10)
}
