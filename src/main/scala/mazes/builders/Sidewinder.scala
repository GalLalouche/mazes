package mazes.builders

import cats.Monad
import cats.data.StateT
import cats.effect.std.Random
import cats.syntax.functor.toFunctorOps
import mazes.data.{Direction, Grid}
import mazes.data.Grid.BasicGrid
import mazes.utils.func.RandomUtils

case object Sidewinder extends MazeGenerator {
  private type Run = List[BasicGrid#Cell]

  def apply[F[_]: Monad: Random](width: Int, height: Int): F[Grid[Unit]] = {
    object Aux extends StateTGeneratorTemplate[F, (BasicGrid, Run)]:
      override protected def template(x: Int, y: Int): StateT[F, (BasicGrid, Run), Unit] = {
        for
          (grid, run) <- StateT.get[F, (BasicGrid, Run)]
          cell = grid(x, y)
          nextRun = cell :: run
          isAtWestWall = cell.go(Direction.East).isEmpty
          _ <- if isAtWestWall then finishRun(nextRun) else continue(nextRun)
        yield ()
      }

      // Choose a random cell in the current run and link it to the its northern neighbor.
      private def finishRun(run: Run): StateT[F, (BasicGrid, Run), Unit] =
        for
          grid <- StateT.inspect[F, (BasicGrid, Run), BasicGrid](_._1)
          chosenOpening <- StateT.liftF(RandomUtils.sampleUnsafe(run))
          nextGrid = grid(chosenOpening.x, chosenOpening.y).link(Direction.North).getOrElse(grid)
          _ <- StateT.set[F, (BasicGrid, Run)](nextGrid -> Nil)
        yield ()

      // If at the top cells, we always continue. Otherwise, we randomly choose whether to continue
      // or not.
      private def continue(run: Run): StateT[F, (BasicGrid, Run), Unit] = {
        val lastAdded = run.head
        val isAtNorthWall = lastAdded.go(Direction.North).isEmpty
        for
          actuallyContinue <- StateT.liftF(RandomUtils.orRandom(isAtNorthWall))
          _ <-
              if actuallyContinue
              then StateT.set(lastAdded.linkUnsafe(Direction.East) -> run)
              else finishRun(run)
        yield ()
      }

    Aux(width, height).runS((Grid(width, height), Nil)).map(_._1)
  }
}