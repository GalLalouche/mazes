package mazes.builders

import cats.Monad
import cats.effect.std.Random
import cats.implicits.toFunctorOps
import mazes.builders.WalkerSt.{GCell, GridB}
import mazes.data.Grid
import mazes.utils.func.RandomUtils
import mazes.utils.func.RandomUtils.sampleUnsafe

case object RandomWalk extends WeavingMazeGeneratorTemplate:
  override def apply[F[_]: Monad: Random](
      width: Int, height: Int, getNeighbors: GCell => Seq[GCell]
  ): F[Grid[Unit]] = {
    object Aux extends Walker[F] {
      override protected def template(grid: GridB, source: GCell): F[(GCell, GridB)] = {
        RandomUtils.sampleUnsafe(getNeighbors(source))
            .map {neighbor =>
              val isVisited = neighbor.value
              (neighbor, if isVisited then grid else link(source, neighbor))
            }
      }
    }
    Aux(width, height)
  }
