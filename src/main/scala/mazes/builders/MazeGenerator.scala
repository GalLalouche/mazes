package mazes.builders

import cats.Monad
import cats.effect.std.Random
import mazes.data.Grid
import mazes.data.Grid.BasicGrid

trait MazeGenerator {
  def apply[F[_]: Monad: Random](width: Int, height: Int): F[BasicGrid]
}
