package mazes.builders

import cats.Monad
import cats.effect.std.Random
import mazes.builders.WalkerSt.GCell
import mazes.data.Grid
import mazes.data.Grid.BasicGrid

abstract class WeavingMazeGeneratorTemplate extends WeavingMazeGenerator {
  def weave[F[_]: Monad: Random](width: Int, height: Int): F[BasicGrid] =
    apply(width, height, _.neighborsWithTunnels)
  def apply[F[_]: Monad: Random](width: Int, height: Int): F[BasicGrid] =
    apply(width, height, _.neighbors)

  protected def apply[F[_]: Monad: Random](
      width: Int, height: Int, getNeighbors: GCell => Seq[GCell]
  ): F[BasicGrid]
}
