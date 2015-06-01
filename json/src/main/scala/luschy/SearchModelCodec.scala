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

import argonaut.StringWrap._
import argonaut.{EncodeJson, Json}

object SearchModelCodec {

  implicit val encodeJsonSearchString: EncodeJson[SearchString] =
    EncodeJson.of[String].contramap(_.x)

  implicit val encodeJsonSize: EncodeJson[Size] =
    EncodeJson.of[Int].contramap(_.x)

  implicit val encodeJsonSkip: EncodeJson[Skip] =
    EncodeJson.of[Int].contramap(_.x)

  implicit val encodeJsonField: EncodeJson[Field] =
    EncodeJson.of[String].contramap(_.x)

  implicit val encodeJsonName: EncodeJson[Name] =
    EncodeJson.of[String].contramap(_.x)

  implicit val encodeJsonSortOrder: EncodeJson[SortOrder] =
    EncodeJson.of[String].contramap(toSnakeCase)

  implicit val encodeJsonFilterType: EncodeJson[FilterType] =
    EncodeJson.of[String].contramap(toSnakeCase)

  implicit val encodeJsonFacetType: EncodeJson[FacetType] =
    EncodeJson.of[String].contramap(toSnakeCase)

  implicit val encodeJsonSort: EncodeJson[Sort] =
    EncodeJson.derive[Sort]

  implicit val encodeJsonSimpleFilter: EncodeJson[SimpleFilter] =
    EncodeJson.derive[SimpleFilter]

  implicit val encodeJsonFilter: EncodeJson[Filter] =
    EncodeJson.derive[Filter]

  implicit val encodeJsonFacet: EncodeJson[Facet] =
    EncodeJson.derive[Facet]

  implicit def encodeJsonSearchResult[A: EncodeJson]: EncodeJson[SearchResult[A]] =
    EncodeJson { sr ⇒
      Json("total" := sr.total, "token" := sr.token, "hits" := sr.docs.map(_.payload2))
    }

  private def toSnakeCase(x: Any) = x.toString
    .replaceAll("([A-Z]+)([A-Z][a-z])", "$1_$2")
    .replaceAll("([a-z\\d])([A-Z])", "$1_$2")
    .toLowerCase

}
