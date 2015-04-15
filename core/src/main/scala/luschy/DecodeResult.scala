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

import shapeless._
import shapeless.syntax.std.tuple._

import collection.generic.CanBuildFrom

sealed trait DecodeResult[A] {

  type E = List[DecodeResult.DecodeError]

  def flatMap[B](f: A ⇒ DecodeResult[B]): DecodeResult[B]

  def map[B](f: A ⇒ B): DecodeResult[B] =
    flatMap(a ⇒ DecodeResult.point(f(a)))

  def ap[B](f: DecodeResult[A ⇒ B]): DecodeResult[B] =
    f.flatMap(ff ⇒ map(a ⇒ ff(a)))

  def |@|[B, C](b: DecodeResult[B]): DecodeResult.Ap2[A, B] =
    new DecodeResult.Ap2(this, b)

  def zip[B](b: DecodeResult[B]): DecodeResult[(A, B)] =
    |@|(b).tupled

  def zipH[B](b: DecodeResult[B]): DecodeResult[A :: B :: HNil] =
    |@|(b).hlist


  def fold[B](f: A ⇒ B)(g: E ⇒ B): B = this match {
    case DecodeResult.Valid(a) ⇒ f(a)
    case DecodeResult.Invalid(h, t) ⇒ g(h :: t)
  }

  def isValid: Boolean =
    fold(_ ⇒ true)(_ ⇒ false)

  def hasErrors: Boolean =
    !isValid

  def exists(f: A => Boolean): Boolean =
    fold(f)(_ ⇒ false)

  def forall(f: A => Boolean): Boolean =
    fold(f)(_ ⇒ true)

  def toOption: Option[A] =
    fold(Some(_): Option[A])(_ ⇒ None)

  def toList: List[A] =
    toOption.toList

  def toEither: Either[E, A] =
    fold(Right(_): Either[E, A])(Left(_))

  def getOrElse[AA >: A](aa: AA): AA =
    fold(x ⇒ x: AA)(_ ⇒ aa)

  def |[AA >: A](aa: AA): AA =
    getOrElse(aa)

  def valueOr[AA >: A](x: E => AA): AA =
    fold(x ⇒ x: AA)(x)

  def orElse[AA >: A](x: DecodeResult[AA]): DecodeResult[AA] =
    fold(DecodeResult.point[AA])(_ ⇒ x)

  def |||[AA >: A](x: DecodeResult[AA]): DecodeResult[AA] =
    orElse(x)
}
object DecodeResult {
  def valid[A](x: A): DecodeResult[A] =
    new Valid(x)

  def point[A](x: A): DecodeResult[A] =
    valid[A](x)

  def missingField[A](field: String): DecodeResult[A] =
    new Invalid(MissingField(field))

  def wrongType[A](field: String, expected: String): DecodeResult[A] =
    new Invalid(WrongType(field, expected))

  def unexpected[A](msg: String): DecodeResult[A] =
    new Invalid(Unexpected(msg))

  def traverse[A, B, F[X] <: TraversableOnce[X], That](xs: F[A])(f: A => DecodeResult[B])(implicit cbf: CanBuildFrom[F[A], B, That]): DecodeResult[That] =
    xs.foldLeft(DecodeResult.point(cbf(xs)))((res, dr) ⇒ f(dr).ap(res.map(a ⇒ a += _))).map(_.result())

  def sequence[A, F[X] <: TraversableOnce[X]](xs: F[DecodeResult[A]])(implicit cbf: CanBuildFrom[F[DecodeResult[A]], A, F[A]]): DecodeResult[F[A]] =
    traverse(xs)(x ⇒ x)

  sealed case class Valid[A](value: A) extends DecodeResult[A] {
    def flatMap[B](f: (A) ⇒ DecodeResult[B]): DecodeResult[B] =
      f(value)

    override def map[B](f: (A) ⇒ B): DecodeResult[B] =
      new Valid(f(value))
  }

  sealed case class Invalid[A](error: DecodeError, additional: List[DecodeError] = Nil) extends DecodeResult[A] {
    def flatMap[B](f: (A) ⇒ DecodeResult[B]): DecodeResult[B] =
      new Invalid(error, additional)

    override def ap[B](f: DecodeResult[(A) ⇒ B]): DecodeResult[B] =
      f match {
        case Valid(_) ⇒ new Invalid(error, additional)
        case Invalid(x, rest) ⇒
          val all = x :: rest ::: error :: additional
          new Invalid(all.head, all.tail)
      }
  }

  sealed trait DecodeError
  sealed case class MissingField(field: String) extends DecodeError
  sealed case class WrongType(field: String, expected: String) extends DecodeError
  sealed case class Unexpected(msg: String) extends DecodeError

  class Ap2[A, B](
    a: DecodeResult[A],
    b: DecodeResult[B]) {
    def apply[C](fn: (A, B) ⇒ C): DecodeResult[C] =
      b.ap(a.map(fn.curried))

    def tupled: DecodeResult[(A, B)] = apply(Tuple2.apply)

    def hlist: DecodeResult[HList.`A, B`.T] =
      tupled.map(_.productElements)

    def |@|[C](c: DecodeResult[C]): Ap3[A, B, C] =
      new Ap3(a, b, c)
  }

  class Ap3[A, B, C](
    a: DecodeResult[A],
    b: DecodeResult[B],
    c: DecodeResult[C]) {
    def apply[D](fn: (A, B, C) ⇒ D): DecodeResult[D] =
      c.ap(b.ap(a.map(fn.curried)))

    def tupled: DecodeResult[(A, B, C)] = apply(Tuple3.apply)

    def hlist: DecodeResult[HList.`A, B, C`.T] =
      tupled.map(_.productElements)

    def |@|[D](d: DecodeResult[D]): Ap4[A, B, C, D] =
      new Ap4(a, b, c, d)
  }

  class Ap4[A, B, C, D](
    a: DecodeResult[A],
    b: DecodeResult[B],
    c: DecodeResult[C],
    d: DecodeResult[D]) {
    def apply[E](fn: (A, B, C, D) ⇒ E): DecodeResult[E] =
      d.ap(c.ap(b.ap(a.map(fn.curried))))

    def tupled: DecodeResult[(A, B, C, D)] = apply(Tuple4.apply)

    def hlist: DecodeResult[HList.`A, B, C, D`.T] =
      tupled.map(_.productElements)

    def |@|[E](e: DecodeResult[E]): Ap5[A, B, C, D, E] =
      new Ap5(a, b, c, d, e)
  }

  class Ap5[A, B, C, D, E](
    a: DecodeResult[A],
    b: DecodeResult[B],
    c: DecodeResult[C],
    d: DecodeResult[D],
    e: DecodeResult[E]) {
    def apply[F](fn: (A, B, C, D, E) ⇒ F): DecodeResult[F] =
      e.ap(d.ap(c.ap(b.ap(a.map(fn.curried)))))

    def tupled: DecodeResult[(A, B, C, D, E)] = apply(Tuple5.apply)

    def hlist: DecodeResult[HList.`A, B, C, D, E`.T] =
      tupled.map(_.productElements)

    def |@|[F](f: DecodeResult[F]): Ap6[A, B, C, D, E, F] =
      new Ap6(a, b, c, d, e, f)
  }

  class Ap6[A, B, C, D, E, F](
    a: DecodeResult[A],
    b: DecodeResult[B],
    c: DecodeResult[C],
    d: DecodeResult[D],
    e: DecodeResult[E],
    f: DecodeResult[F]) {
    def apply[G](fn: (A, B, C, D, E, F) ⇒ G): DecodeResult[G] =
      f.ap(e.ap(d.ap(c.ap(b.ap(a.map(fn.curried))))))

    def tupled: DecodeResult[(A, B, C, D, E, F)] = apply(Tuple6.apply)

    def hlist: DecodeResult[HList.`A, B, C, D, E, F`.T] =
      tupled.map(_.productElements)

    def |@|[G](g: DecodeResult[G]): Ap7[A, B, C, D, E, F, G] =
      new Ap7(a, b, c, d, e, f, g)
  }

  class Ap7[A, B, C, D, E, F, G](
    a: DecodeResult[A],
    b: DecodeResult[B],
    c: DecodeResult[C],
    d: DecodeResult[D],
    e: DecodeResult[E],
    f: DecodeResult[F],
    g: DecodeResult[G]) {
    def apply[H](fn: (A, B, C, D, E, F, G) ⇒ H): DecodeResult[H] =
      g.ap(f.ap(e.ap(d.ap(c.ap(b.ap(a.map(fn.curried)))))))

    def tupled: DecodeResult[(A, B, C, D, E, F, G)] = apply(Tuple7.apply)

    def hlist: DecodeResult[HList.`A, B, C, D, E, F, G`.T] =
      tupled.map(_.productElements)

    def |@|[H](h: DecodeResult[H]): Ap8[A, B, C, D, E, F, G, H] =
      new Ap8(a, b, c, d, e, f, g, h)
  }

  class Ap8[A, B, C, D, E, F, G, H](
    a: DecodeResult[A],
    b: DecodeResult[B],
    c: DecodeResult[C],
    d: DecodeResult[D],
    e: DecodeResult[E],
    f: DecodeResult[F],
    g: DecodeResult[G],
    h: DecodeResult[H]) {
    def apply[I](fn: (A, B, C, D, E, F, G, H) ⇒ I): DecodeResult[I] =
      h.ap(g.ap(f.ap(e.ap(d.ap(c.ap(b.ap(a.map(fn.curried))))))))

    def tupled: DecodeResult[(A, B, C, D, E, F, G, H)] = apply(Tuple8.apply)

    def hlist: DecodeResult[HList.`A, B, C, D, E, F, G, H`.T] =
      tupled.map(_.productElements)

    def |@|[I](i: DecodeResult[I]): Ap9[A, B, C, D, E, F, G, H, I] =
      new Ap9(a, b, c, d, e, f, g, h, i)
  }

  class Ap9[A, B, C, D, E, F, G, H, I](
    a: DecodeResult[A],
    b: DecodeResult[B],
    c: DecodeResult[C],
    d: DecodeResult[D],
    e: DecodeResult[E],
    f: DecodeResult[F],
    g: DecodeResult[G],
    h: DecodeResult[H],
    i: DecodeResult[I]) {

    def apply[J](fn: (A, B, C, D, E, F, G, H, I) ⇒ J): DecodeResult[J] =
      i.ap(h.ap(g.ap(f.ap(e.ap(d.ap(c.ap(b.ap(a.map(fn.curried)))))))))

    def tupled: DecodeResult[(A, B, C, D, E, F, G, H, I)] = apply(Tuple9.apply)

    def hlist: DecodeResult[HList.`A, B, C, D, E, F, G, H, I`.T] =
      tupled.map(_.productElements)
  }
}



