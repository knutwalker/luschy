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

import argonaut._
import validation.Result
import validation.Result.syntax._
import validation.Result.{invalid, valid}

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable

trait RequestPartDecoder[A] {

  def decode(c: HCursor): (Field, String) \@/ A

  def apply(c: HCursor): (Field, String) \@/ A =
    decode(c)

  def decodeJson(j: Json): (Field, String) \@/ A =
    decode(j.hcursor)


  def map[B](f: A => B): RequestPartDecoder[B] = {
    val thisDecode = decode _
    new RequestPartDecoder[B] {
      def decode(c: HCursor): ((Field, String) \@/ B) =
        thisDecode(c).map(f)
    }
  }

  def catMap[B](f: A => (Field, String) \@/ B): RequestPartDecoder[B] = {
    val thisDecode = decode _
    new RequestPartDecoder[B] {
      def decode(c: HCursor): (Field, String) \@/ B =
        thisDecode(c).flatMap(f)
    }
  }

  def |||[AA >: A](x: => RequestPartDecoder[AA]): RequestPartDecoder[AA] = {
    RequestPartDecoder[AA] { c =>
      val q: (Field, String) \@/ AA = decode(c)
      q orElse x.decode(c)
    }
  }
}
object RequestPartDecoder {

  def apply[A](f: HCursor => (Field, String) \@/ A): RequestPartDecoder[A] =
    new RequestPartDecoder[A] {
      def decode(c: HCursor): (Field, String) \@/ A =
        f(c)
    }

  @inline def of[A](implicit A: RequestPartDecoder[A]): RequestPartDecoder[A] = A

  implicit def cbfRequestPartDecoder[A, C[_]](implicit A: RequestPartDecoder[A], cbf: CanBuildFrom[Nothing, A, C[A]]): RequestPartDecoder[C[A]] =
    new RequestPartDecoder[C[A]] {
      def decode(c: HCursor): (Field, String) \@/ C[A] = {
        c.downArray.hcursor match {
          case None =>
            if (c.focus.isArray)
              cbf().result().valid
            else
              invalid((fieldFromHistory(c.history) | Field("Unknown"), "Could not decode as List"))
          case Some(h) =>
            go(h, Result.valid(cbf())).map(_.result())
        }
      }

      @tailrec
      private def go(hc: HCursor, res: (Field, String) \@/ mutable.Builder[A, C[A]]): (Field, String) \@/ mutable.Builder[A, C[A]] = {
        val newRes = A.decode(hc).apply(res.map(b => b += (_: A)))
        hc.right.hcursor match {
          case None => newRes
          case Some(nc) => go(nc, newRes)
        }
      }
    }

  implicit def optionRequestPartDecoder[A](implicit A: RequestPartDecoder[A]): RequestPartDecoder[Option[A]] = {
    new RequestPartDecoder[Option[A]] {
      def decode(c: HCursor): (Field, String) \@/ Option[A] = {
        c.acursor.success match {
          case None => none[A].valid
          case Some(v) =>
            if (v.focus.isNull) {
              none[A].valid
            } else {
              A.decode(v).map(_.?)
            }
        }
      }
    }
  }

  implicit def fromDecodeJson[A](implicit A: DecodeJson[A]): RequestPartDecoder[A] =
    new RequestPartDecoder[A] {
      def decode(c: HCursor): (Field, String) \@/ A = {
        A.decode(c).fold({
          case ("[A]List[A]", _) ⇒
            invalid((Field("Unknown"), "Could not decode as List"))
          case (m, GetField(f)) ⇒
            invalid((f, s"Could not decode [${c.focus}] as [$m]"))
          case (m, h) ⇒
            invalid((Field("unknown"), m))
        }, valid)
      }
    }


  def fieldFromHistory(c: CursorHistory): Option[Field] = c match {
    case CursorHistory(El(CursorOpDownField(f), true) :: _) ⇒ Field(f).some
    case CursorHistory(El(CursorOpDownArray, true) :: El(CursorOpDownField(f), true) :: _) ⇒ Field(f).some
    case _ => None
  }
  object GetField {
    def unapply(c: CursorHistory): Option[Field] =
      fieldFromHistory(c)
  }
}
