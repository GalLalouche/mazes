package mazes.utils.func

import cats.{Applicative, Functor}
import cats.effect.std.Random
import cats.syntax.functor.toFunctorOps

object RandomUtils {
  def sample[F[_]: Random: Applicative, A](xs: Seq[A]): F[Option[A]] =
    if xs.isEmpty
    then Applicative[F].pure(None)
    else Random[F].nextIntBounded(xs.size).map(i => Some(xs(i)))
  def sampleUnsafe[F[_]: Random: Functor, A](xs: Seq[A]): F[A] =
    Random[F].nextIntBounded(xs.size).map(xs)
  /** Only rolls a random Boolean if the input is [[false]] */
  def orRandom[F[_]: Random: Applicative](b: Boolean): F[Boolean] =
    if b then Applicative[F].pure(b) else Random[F].nextBoolean
}
