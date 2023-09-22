package mazes.builders

import cats.Monad
import cats.effect.std.Random
import mazes.data.Grid
import mazes.data.Grid.BasicGrid

trait WeavingMazeGenerator extends MazeGenerator {
  def weave[F[_]: Monad: Random](width: Int, height: Int): F[BasicGrid]
  def asRegularGenerator: MazeGenerator = new MazeGenerator:
    override def apply[F[_]: Monad: Random](width: Int, height: Int) = weave(width, height)
    override val toString = s"${WeavingMazeGenerator.this.toString} (weaving)"
}
