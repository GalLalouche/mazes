package mazes.builders

import cats.{Applicative, Monad}
import cats.effect.std.Random
import cats.implicits.{toFlatMapOps, toFunctorOps}
import mazes.data.Grid

private trait Walker[F[_]: Monad: Random] {
  protected type GridB = Grid[Boolean]
  protected type GCell = GridB#Cell
  def apply(width: Int, height: Int): F[Grid[Unit]] = {
    def go(x: Int, y: Int, result: Grid[Boolean], count: Int): F[Grid[Boolean]] = {
      if count == result.size then Applicative[F].pure(result) else {
        template(result, result(x, y)).flatMap((newCell, newGrid) =>
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
      x0 <- Random[F].nextIntBounded(width)
      y0 <- Random[F].nextIntBounded(height)
      result <- go(x0, y0, base(x0, y0).set(true), 1)
    yield result.as(())
  }

  def link(src: GCell, dst: GCell): GridB = src.linkToUnsafe(dst)(dst.x, dst.y).set(true)

  protected def template(grid: GridB, source: GCell): F[(GCell, GridB)]
}
