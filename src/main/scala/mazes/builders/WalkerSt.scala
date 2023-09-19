package mazes.builders

import cats.{Applicative, Monad}
import cats.data.StateT
import cats.effect.std.Random
import cats.implicits.toFunctorOps
import mazes.builders.WalkerSt.{GCell, GridB}
import mazes.data.Grid

private trait WalkerSt[F[_]: Monad, St] {
  def apply(width: Int, height: Int, random: Random[F]): StateT[F, St, Grid[Unit]] = {
    def go(x: Int, y: Int, result: Grid[Boolean], count: Int): StateT[F, St, Grid[Boolean]] = {
      if count == result.size then Applicative[StateT[F, St, _]].pure(result) else {
        template(result, result(x, y), random).flatMap((newCell: GCell, newGrid) =>
          go(
            newCell.x,
            newCell.y,
            newGrid,
            count + (if newGrid.eq(result) then 0 else 1)
          )
        )
      }
    }
    val base = Grid(width, height).as[Boolean](false)
    for
      x0 <- StateT.liftF(random.nextIntBounded(width))
      y0 <- StateT.liftF(random.nextIntBounded(height))
      result <- go(x0, y0, base, 0)
    yield result.as(())
  }

  protected def template(grid: GridB, src: GCell, random: Random[F]): StateT[F, St, (GCell, GridB)]
}

object WalkerSt {
  type GridB = Grid[Boolean]
  type GCell = GridB#Cell
}