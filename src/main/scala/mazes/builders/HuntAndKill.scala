package mazes.builders

import cats.Monad
import cats.effect.std.Random
import cats.implicits.{toFlatMapOps, toFunctorOps}
import mazes.builders.WalkerSt.{GCell, GridB}
import mazes.data.Grid
import mazes.utils.func.RandomUtils

case object HuntAndKill extends WeavingMazeGeneratorTemplate {
  override protected def apply[F[_]: Monad: Random](
      width: Int, height: Int, getNeighbors: GCell => Seq[GCell]
  ): F[Grid[Unit]] = {
    object Aux extends Walker[F] {
      override protected def template(result: GridB, source: GCell): F[(GCell, GridB)] = {
        val neighbors: Seq[GCell] = getNeighbors(source).filterNot(_.value)
        if neighbors.isEmpty
        then // Hunt mode
          val unvisitedWithVisitedNeighbors =
            result.cells.filter(e => !e.value && getNeighbors(e).exists(_.value))
          for
            prey: GCell <- RandomUtils.sampleUnsafe(unvisitedWithVisitedNeighbors)
            visitedNeighbor <- RandomUtils.sampleUnsafe(getNeighbors(prey).filter(_.value))
          yield (prey, link(visitedNeighbor, prey))
        else
          RandomUtils.sampleUnsafe(neighbors).map(nextNeighbor => {
            val x = nextNeighbor.x
            val y = nextNeighbor.y
            val value = result(source.x, source.y).linkToUnsafe(nextNeighbor)
            (nextNeighbor, value.apply(x, y).set(true))
          })
      }
    }
    Aux(width, height)
  }
}
