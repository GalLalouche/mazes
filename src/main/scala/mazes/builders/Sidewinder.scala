package mazes.builders

import cats.Monad
import cats.data.StateT
import cats.effect.std.Random
import cats.syntax.functor.toFunctorOps
import cats.syntax.traverse.toTraverseOps
import mazes.data.{Direction, Grid}
import mazes.utils.func.RandomUtils.*

object Sidewinder {
  private type Run = Seq[Grid#Cell]

  def apply[F[_] : Monad](width: Int, height: Int, random: Random[F]): F[Grid] =
    new Aux(width, height, random).go

  private class Aux[F[_] : Monad](width: Int, height: Int, random: Random[F]) {
    def go = {
      Grid.coordinates(height = height, width = width).traverse {
        (x, y) =>
          for
            (grid, run) <- StateT.get[F, (Grid, Run)]
            cell = grid(x, y)
            nextRun = run :+ cell
            isAtWestWall = cell.go(Direction.East).isEmpty
            _ <- if isAtWestWall then finishRun(nextRun) else continue(nextRun)
          yield ()
      }.runS(new Grid(width, height) -> Vector.empty).map(_._1)
    }

    private def finishRun(run: Run): StateT[F, (Grid, Run), Unit] = {
      assert(run.nonEmpty)
      for
        grid <- StateT.inspect[F, (Grid, Run), Grid](_._1)
        chosenOpening <- StateT.liftF(random.sampleUnsafe(run))
        nextGrid = grid(chosenOpening.x, chosenOpening.y).link(Direction.North).getOrElse(grid)
        _ <- StateT.set(nextGrid -> Vector.empty)
      yield ()
    }

    private def continue(run: Run): StateT[F, (Grid, Run), Unit] = {
      val last = run.last
      val isAtNorthWall = last.go(Direction.North).isEmpty
      for
        actuallyContinue <- StateT.liftF(random.nextBoolean).map(_ || isAtNorthWall)
        _ <-
            if actuallyContinue
            then StateT.set((last.linkUnsafe(Direction.East)) -> run)
            else finishRun(run)
      yield ()
    }
  }
}