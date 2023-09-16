package mazes.utils.func

import cats.effect.std.Random
import cats.Applicative
import cats.syntax.functor.toFunctorOps

object RandomUtils {
  extension[F[_]] (random: Random[F])(using app: Applicative[F])
    def sample[A](xs: Seq[A]): F[Option[A]] =
      if xs.isEmpty then app.pure(None) else random.nextIntBounded(xs.size).map(i => Some(xs(i)))
    def sampleUnsafe[A](xs: Seq[A]): F[A] = random.nextIntBounded(xs.size).map(xs)
}
