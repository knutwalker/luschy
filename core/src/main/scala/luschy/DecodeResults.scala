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

import validation.{Mergeable, Result}

object DecodeResults {

  def valid[A](x: A): DecodeResult[A] =
    Result.valid(x)

  def missingField[A](field: String): DecodeResult[A] =
    Result.invalid(MissingField(field))

  def wrongType[A](field: String, expected: String): DecodeResult[A] =
    Result.invalid(WrongType(field, expected))

  def unexpected[A](msg: String): DecodeResult[A] =
    Result.invalid(Unexpected(msg))

  sealed trait DecodeError
  sealed case class MissingField(field: String) extends DecodeError
  sealed case class WrongType(field: String, expected: String) extends DecodeError
  sealed case class Unexpected(msg: String) extends DecodeError
  sealed case class Multiple(errors: Vector[DecodeError]) extends DecodeError

  object DecodeError {
    implicit val decodeErrorMergable: Mergeable[DecodeError] =
      Mergeable.instance((a, b) ⇒ a match {
        case Multiple(xs) ⇒ b match {
          case Multiple(ys) ⇒ Multiple(xs ++ ys)
          case y            ⇒ Multiple(xs :+ y)
        }
        case x            ⇒ b match {
          case Multiple(ys) ⇒ Multiple(b +: ys)
          case y            ⇒ Multiple(Vector(x, y))
        }
      })
  }
}
