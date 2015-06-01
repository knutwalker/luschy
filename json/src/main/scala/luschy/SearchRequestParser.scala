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

import validation.Result
import validation.Result.invalid
import validation.Result.syntax._
import validation.Result.symbolic._

import argonaut._
import argonaut.Json._
import argonaut.StringWrap._


abstract class SearchRequestParser[A] {
  import SearchRequestParser._

  def config: SearchRequestParserConfig[A]

  def apply(json: Json): (Field, String) \@/ A =
    decodeSearchRequest(json.hcursor)

  def parseJson(json: Json): Json \@/ A =
    decodeSearchRequest(json.hcursor).fold(
      es ⇒ invalid(errorsAsJson(s"Could not decode [$json] as a ${config.name}", es.toList)),
      Result.valid)

  def decodeSearchRequest(c: HCursor): (Field, String) \@/ A = {
    val cfg = config
    import cfg._

    val query =
      decodeAsOption[String](queryField, additionalQueryFields: _*)(c)
        .map(_.filterNot(_.isEmpty))

    val size =
      decodeAsOption[Int](sizeField, additionalSizeFields: _*)(c)
        .filter(_.forall(_ >= 0), (Field(sizeField), "value must be positive"))

    val skip =
      decodeAsOption[Int](skipField, additionalSkipFields: _*)(c)
        .filter(_.forall(_ >= 0), (Field(skipField), "value must be positive"))

    val default =
      decodeAsOption[String](defaultFieldField, additionalDefaultFieldFields: _*)(c)

    val sort =
      decodeAsList[Sort](sortField, additionalSortFields: _*)(c)
        .getOrElse(Result.valid(Nil))

    val filter =
      decodeAsList[Filter](filterField, additionalFilterFields: _*)(c)
        .getOrElse(Result.valid(Nil))

    val token =
      decodeAsOption[Option[Long]](tokenField, additionalTokenFields: _*)(c)
        .map(_.flatten)

    (query |@| size |@| skip |@| default |@| sort |@| filter |@| token).apply { (q, s, k, d, ss, fs, t) ⇒
        config(
          q.map(SearchString),
          d.map(Field),
          s.map(Size(_)),
          k.map(Skip(_)),
          ss,
          fs,
          t
        ).invalidMap(s => (Field(config.name), s))
      }.flatMap(identity)
  }
}
object SearchRequestParser {
  import SearchModelCodec._

  private type DecodedOne[+A] = (Field, String) \@/ A
  private type DecodedMany[+A] = (Field, String) \@/ List[A]
  private type DecodingOne[+A] = Option[(Field, String) \@/ A]
  private type DecodingMany[+A] = Option[(Field, String) \@/ List[A]]

  private val sorts  = """(\+|-)?(\w+)(?::(asc|desc))?""".r
  private val filters  = """(\w+)(?::)(.+)""".r

  trait SearchRequestParserConfig[A] {

    def name: String

    def queryField: String =
      "query"
    def additionalQueryFields: Seq[String] =
      List("q")

    def defaultFieldField: String =
      "default_field"
    def additionalDefaultFieldFields: Seq[String] =
      List("defaultField", "df")

    def sizeField: String =
      "size"
    def additionalSizeFields: Seq[String] =
      List("s", "rows", "limit")

    def skipField: String =
      "skip"
    def additionalSkipFields: Seq[String] =
      List("from")

    def sortField: String =
      "sort"
    def additionalSortFields: Seq[String] =
      List("order", "order_by", "orderBy")

    def filterField: String =
      "filter"
    def additionalFilterFields: Seq[String] =
      List("f", "filters", "fs")

    def tokenField: String =
      "token"
    def additionalTokenFields: Seq[String] =
      Nil

    def apply(
      query: Option[SearchString],
      defaultField: Option[Field],
      size: Option[Size],
      skip: Option[Skip],
      sort: List[Sort],
      filter: List[Filter],
      token: Option[Long])
    : String \@/ A

  }

  private def errorsAsJson(msg: String, details: List[(Field, String)]): Json = {
    val detailGroups = details
      .groupBy(_._1.x)
      .mapValues(_.map(_._2))
    val detailJson: List[Json] = detailGroups.collect {
      case (f, e :: Nil)  ⇒ Json.obj("field" → jString(f), "error" → jString(e))
      case (f, e :: rest) ⇒ Json.obj("field" → jString(f), "errors" → jArray((e :: rest).map(jString)))
    }(collection.breakOut)
    Json.obj("error" → jString(msg), "details" → jArray(detailJson))
  }

  private def decodeAsOption[A: RequestPartDecoder](f: String, fs: String*)(c: HCursor): DecodedOne[Option[A]] =
    decodeAsList[A](f, fs: _*)(c)
      .map(_.map(_.lastOption))
      .getOrElse(Result.valid(None))

  private def decodeAsList[A: RequestPartDecoder](f: String, fs: String*)(c: HCursor): DecodingMany[A] =
    decodeAsList0[A](f :: fs.toList, c, None).map(_.invalidMap(x ⇒ (Field(f), x._2)))

  @annotation.tailrec
  private def decodeAsList0[A: RequestPartDecoder](fs: List[String], c: HCursor, res: DecodingMany[A]): DecodingMany[A] = fs match {
    case f :: rest ⇒ decodeAsList0[A](rest, c, mergeDecodings[A](res, decodeField(f, c)(decodeOneOrMany[A])))
    case Nil       ⇒ res
  }

  private def mergeDecodings[A](d1: DecodingMany[A], d2: ⇒ DecodingMany[A]): DecodingMany[A] = {
    // early terminate with d1.exists(_.isValid)
    // prefer failures with r1 +++ r2
    val res2 = d2
    d1.flatMap(r1 ⇒ res2.map(r2 ⇒ (r1 +|+ r2).map(_.toList.flatten))).orElse(d1).orElse(res2)
  }

  private def decodeField[A: RequestPartDecoder](f: String, c: HCursor)(d: HCursor ⇒ DecodedMany[A]): DecodingMany[A] =
    c.downField(f).hcursor.map(d)

  private def decodeOneOrMany[A: RequestPartDecoder](c: HCursor): DecodedMany[A] =
    decodeOne[A](c).fold(
      es ⇒ decodeOne[List[A]](c).fold(fs => {
        if (fs.head._2 == "Could not decode as List") {
          Result.invalids(es)
        } else {
          Result.invalids(fs)
        }
      },
      x => Result.valid(x)
      ),
      xs ⇒ Result.valid(List(xs))
    )

  private def decodeOne[A](c: HCursor)(implicit A: RequestPartDecoder[A]): DecodedOne[A] =
    A.decode(c)

  private implicit val requestPartDecoderString: RequestPartDecoder[String] =
    RequestPartDecoder.fromDecodeJson(DecodeJson.StringDecodeJson)

  private val requestPartDecoderSortByString: RequestPartDecoder[Sort] =
    requestPartDecoderString.catMap {
      case x@sorts(pre, field, suff) =>
        if ((pre eq null) && (suff eq null)) {
          Sort(Field(field), SortOrder.Asc).valid
        } else if ((pre ne null) && (suff eq null)) {
          val sort = Option(pre).collect {
            case "+" ⇒ Sort(Field(field), SortOrder.Asc).valid
            case "-" ⇒ Sort(Field(field), SortOrder.Desc).valid
          }
          sort.getOrElse((Field("sort"), s"prefix [$pre] must be either [+] or [-]").invalid)
        } else if ((suff ne null) && (pre eq null)) {
          val sort = Option(suff).collect {
            case "asc"  ⇒ Sort(Field(field), SortOrder.Asc).valid
            case "desc" ⇒ Sort(Field(field), SortOrder.Desc).valid
          }
          sort.getOrElse((Field("sort"), s"suffix [$pre] must be either [asc] or [desc]").invalid)
        } else {
          invalid((Field("sort"), s"Ambiguous prefix and suffix qualifier provided for [$x]"))
        }
      case x =>
        Sort(Field(x), SortOrder.Asc).valid
    }

  private val requestPartDecoderSortByObject: RequestPartDecoder[Sort] =
    RequestPartDecoder(c ⇒ {
      val fieldRes = (c --\ "field").hcursor match {
        case None    ⇒ invalid("Missing field: 'field'")
        case Some(f) ⇒ requestPartDecoderString.decode(f).bimap(_._2, Field)
      }
      val orderRes = (c --\ "order").hcursor match {
        case None    ⇒ SortOrder.Asc.valid
        case Some(f) ⇒ requestPartDecoderString.decode(f)
          .bimap(_._2, _.toLowerCase).flatMap {
            case "asc"  ⇒ SortOrder.Asc.valid
            case "desc" ⇒ SortOrder.Desc.valid
            case ff     ⇒ invalid(s"'order' [$ff] must be either [asc] or [desc]")
          }
      }
      val sortRes = (fieldRes and orderRes) apply Sort
      sortRes.invalidMap((Field("sort"), _))
    })

  private implicit val requestPartDecoderSort: RequestPartDecoder[Sort] =
    requestPartDecoderSortByString ||| requestPartDecoderSortByObject


  val requestPartDecoderFilterByString: RequestPartDecoder[Filter] =
    requestPartDecoderString.catMap {
      case filters(field, value) =>
        Filter(Name(field), List(value)).valid
      case x =>
        Result.invalid((Field("filter"), s"filter string must be of form '<field>:<value>'"))
    }

  private val requestPartDecoderFilterByObject: RequestPartDecoder[Filter] =
    new RequestPartDecoder[Filter] {
      def continue(f: Option[String], nameRes: String \@/ Name, c: HCursor): DecodedOne[Filter] = {
        val firstKey = f.getOrElse("value")
        val keys = f.toSeq ++ Seq("value", "key", "values", "keys")
        val valueRes = decodeAsList[String](keys.head, keys.tail: _*)(c)
          .map(_.invalidMap(_._2))
          .getOrElse(invalid(s"Missing field: '$firstKey'"))
          .filter(_.nonEmpty, s"'$firstKey' must have at lease one value.")

        val filterRes = (nameRes and valueRes) apply (Filter(_, _))
        filterRes.invalidMap((Field("filter"), _))
      }

      def decode(c: HCursor): DecodedOne[Filter] = (c --\ "field").hcursor match {
        case None    ⇒
          c.fields match {
            case Some(f :: Nil) =>
              continue(Some(f), Name(f).valid, c)
            case Some(Nil) =>
              continue(None, invalid(s"No fields given in [${c.focus}]"), c)
            case Some(more) =>
              continue(None, invalid(s"Multiple fields given: [${more.mkString(", ")}]"), c)
            case None =>
              continue(None, invalid("Missing field: 'field'"), c)
          }
        case Some(f) ⇒
          val res = requestPartDecoderString.decode(f).bimap(_._2, Name)
          continue(None, res, c)
      }
    }

  private implicit val requestPartDecoderFilter: RequestPartDecoder[Filter] =
    requestPartDecoderFilterByString ||| requestPartDecoderFilterByObject

  implicit val encodeJsonSearchRequest: EncodeJson[SearchRequest] =
    EncodeJson(sr ⇒
      ("query" := sr.query) ->:
        ("size" := sr.size) ->:
        ("skip" := sr.skip) ->:
        ("default_field" := sr.defaultField) ->:
        ("sort" := sr.sort) ->:
        ("pre_filter" := sr.preFilter) ->:
        ("filter" := sr.filter) ->:
        ("facets" := sr.facets) ->:
        ("token" := sr.token) ->:
        jEmptyObject
    )
}
