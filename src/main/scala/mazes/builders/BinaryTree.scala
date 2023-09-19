package mazes.builders

import cats.Monad
import cats.data.StateT
import cats.effect.std.Random
import cats.implicits.toFlatMapOps
import mazes.data.{Direction, Grid}
import mazes.data.Grid.BasicGrid
import mazes.utils.func.RandomUtils
import mazes.utils.func.RandomUtils.sample

case object BinaryTree extends MazeGenerator {
  def apply[F[_]: Monad: Random](width: Int, height: Int): F[Grid[Unit]] = {
    object Aux extends StateTGeneratorTemplate[F, BasicGrid] {
      override protected def template(x: Int, y: Int): StateT[F, BasicGrid, Unit] = {
        for
          grid <- StateT.get[F, BasicGrid]
          cell = grid(x, y)
          nextGrids = Vector(cell.link(Direction.North), cell.link(Direction.East)).flatten
          nextGrid <- StateT.liftF(RandomUtils.sample(nextGrids))
          _ <- StateT.set(nextGrid.getOrElse(grid))
        yield ()
      }
    }
    Aux(width, height).runS(Grid(width, height))
  }
}