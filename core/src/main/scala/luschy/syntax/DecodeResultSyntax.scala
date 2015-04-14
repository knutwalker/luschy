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
package syntax


trait DecodeResultSyntax {

  implicit final class DecodeResultOps[A](val x: A) {
    def valid: DecodeResult[A] = DecodeResult.valid[A](x)
  }

  implicit final class OptionDecodeResultOps[A](val x: Option[A]) {
    def toDecodeResult(e: => DecodeResult.DecodeError): DecodeResult[A] = x match {
      case Some(v) ⇒ DecodeResult.valid[A](v)
      case None    ⇒ DecodeResult.Invalid(e)
    }
  }
}
object DecodeResultSyntax extends DecodeResultSyntax
