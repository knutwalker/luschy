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

import validation.Result.syntax._

object SimpleSearchRequestCodec extends SearchRequestParser[SimpleSearchRequest] {
  object config extends SearchRequestParser.SearchRequestParserConfig[SimpleSearchRequest] {
    def name: String = "SimpleSearchRequest"
    def apply(
      query: Option[SearchString],
      defaultField: Option[Field],
      size: Option[Size],
      skip: Option[Skip],
      sort: List[Sort],
      filter: List[Filter],
      token: Option[Long]): String \@/ SimpleSearchRequest = {

      SimpleSearchRequest(
        query | SearchString("*:*"),
        size | Size.ten,
        skip | Skip.zero,
        defaultField | Field("_all"),
        sort,
        filter.map(f => SimpleFilter(f.name, f.values)),
        token,
        Some(System.currentTimeMillis())
      ).valid
    }
  }
}
