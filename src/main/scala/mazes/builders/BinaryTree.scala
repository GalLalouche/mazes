package mazes.builders

import cats.Monad
import cats.data.StateT
import cats.effect.std.Random
import cats.syntax.traverse.toTraverseOps
import mazes.data.{Direction, Grid}
import mazes.utils.func.RandomUtils.sample

object BinaryTree {
  def apply[F[_] : Monad](width: Int, height: Int, random: Random[F]): F[Grid] = {
    Grid.coordinates(height = height, width = width).traverse((x, y) =>
      for
        grid: Grid <- StateT.get
        cell = grid(x, y)
        directions = Vector(cell.link(Direction.North), cell.link(Direction.East)).flatten
        next <- StateT.liftF(random.sample(directions))
        _ <- StateT.set(next.getOrElse(grid))
      yield ()
    ).runS(new Grid(width, height))
  }
}