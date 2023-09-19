package mazes.builders

import cats.Monad
import cats.data.StateT
import cats.effect.std.Random
import cats.implicits.toFunctorOps
import cats.syntax.traverse.toTraverseOps
import mazes.data.Grid

private trait StateTGeneratorTemplate[F[_]: Monad: Random, St] {
  def apply(width: Int, height: Int): StateT[F, St, Unit] =
    Grid.coordinates(height = height, width = width).traverse(template).as(())

  protected def template(x: Int, y: Int): StateT[F, St, Unit]
}
